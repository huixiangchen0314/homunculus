(ns top.kzre.homunculus.core.types.elaborate.core
  "IR2 闭包消除 pass（Elaborate）。将残余的 :lambda 节点消除，使 IR2 变为一阶。"
  (:require [top.kzre.homunculus.core.types.elaborate.protocol :as cfg]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :as lift]
            [clojure.set :as set]))

;; ── 辅助：自由变量分析 ──────────────────────────
(defn free-vars [lambda-node]
  (let [bound (set (map :name (:params lambda-node)))]
    (set/difference (lift/free-vars (:body lambda-node)) bound)))

;; ── 查找顶层函数定义 ──────────────────────────
(defn find-toplevel-define [ir2-roots name]
  (some (fn [root]
          (when (and (satisfies? ir2p/INode root)
                     (= (ir2p/kind root) :define)
                     (= (:name root) name))
            root))
        ir2-roots))

(defn fresh-name [base]
  (symbol (str (name base) "_" (gensym ""))))

;; ── 树替换（仅针对根列表中指针相等的节点） ──
(defn replace-node [tree old new]
  (if (identical? tree old)
    new
    (if (satisfies? ir2p/INode tree)
      (let [new-children (mapv #(replace-node % old new) (ir2p/children tree))]
        (clojure.core/assoc tree :children new-children))
      tree)))

;; ── 内联 let 绑定中的闭包 ─────────────────────
;; 注意：这里仅处理闭包作为绑定值、且变量在 body 中被调用的简单情况。
;; 更复杂的情况（如变量被传递）需由单态化处理。
(defn- subst-var
  "在 node 树中，将所有对变量 var-name 的引用替换为 replacement。"
  [node var-name replacement]
  (if (and (satisfies? ir2p/INode node)
           (= (ir2p/kind node) :variable)
           (= (:name node) var-name))
    replacement
    (case (ir2p/kind node)
      :call
      (let [new-fn (subst-var (:fn node) var-name replacement)
            new-args (mapv #(subst-var % var-name replacement) (:args node))]
        (clojure.core/assoc node :fn new-fn :args new-args))
      :let
      (let [new-bindings (mapv (fn [[v val]] [(subst-var v var-name replacement) (subst-var val var-name replacement)]) (:bindings node))
            new-body (subst-var (:body node) var-name replacement)]
        (clojure.core/assoc node :bindings new-bindings :body new-body))
      :block
      (let [new-exprs (mapv #(subst-var % var-name replacement) (:exprs node))]
        (clojure.core/assoc node :exprs new-exprs))
      :if
      (let [new-test (subst-var (:test node) var-name replacement)
            new-then (subst-var (:then node) var-name replacement)
            new-else (when (:else node) (subst-var (:else node) var-name replacement))]
        (clojure.core/assoc node :test new-test :then new-then :else new-else))
      :loop
      (let [new-bindings (mapv (fn [[v val]] [(subst-var v var-name replacement) (subst-var val var-name replacement)]) (:bindings node))
            new-body (subst-var (:body node) var-name replacement)]
        (clojure.core/assoc node :bindings new-bindings :body new-body))
      :assign
      (let [new-var (subst-var (:var node) var-name replacement)
            new-val (subst-var (:val node) var-name replacement)]
        (clojure.core/assoc node :var new-var :val new-val))
      ;; 默认仅更新 children
      (let [new-children (mapv #(subst-var % var-name replacement) (ir2p/children node))]
        (clojure.core/assoc node :children new-children)))))

(defn inline-let-binding [let-node]
  (let [bindings (:bindings let-node)
        body (:body let-node)]
    (loop [remaining bindings
           new-bindings []
           new-body body]
      (if-let [[var val] (first remaining)]
        (if (and (satisfies? ir2p/INode val)
                 (= (ir2p/kind val) :lambda))
          ;; 内联：将 body 中对 var 的引用直接替换为闭包体
          (let [new-body' (subst-var new-body (:name var) val)]
            (recur (rest remaining) new-bindings new-body'))
          (recur (rest remaining) (conj new-bindings [var val]) new-body))
        (if (seq new-bindings)
          (assoc let-node :bindings (vec new-bindings) :body new-body)
          new-body)))))

;; ── 上下文收集 ──────────────────────────────
(defn- lambda-contexts [ir2-roots]
  (let [ctxs (atom [])]
    (letfn [(walk [node parent role index]
              (when (satisfies? ir2p/INode node)
                (when (and (= (ir2p/kind node) :lambda)
                           ;; 跳过顶层 define 内部的 lambda
                           (not (and parent (= (ir2p/kind parent) :define))))
                  (swap! ctxs conj {:lambda node :parent parent :role role :index index}))
                (case (ir2p/kind node)
                  :call
                  (do (walk (:fn node) node :fn nil)
                      (doseq [[i arg] (map-indexed vector (:args node))]
                        (walk arg node :args i)))
                  :let
                  (do (doseq [[i [var val]] (map-indexed vector (:bindings node))]
                        (walk var node :let-var i)
                        (walk val node :let-val i))
                      (walk (:body node) node :body nil))
                  :block
                  (doseq [[i expr] (map-indexed vector (:exprs node))]
                    (walk expr node :block-expr i))
                  :if
                  (do (walk (:test node) node :test nil)
                      (walk (:then node) node :then nil)
                      (when (:else node) (walk (:else node) node :else nil)))
                  :lambda
                  (walk (:body node) node :lambda-body nil)
                  ;; 其他节点通过 children 遍历
                  (doseq [c (ir2p/children node)]
                    (walk c node :child nil)))))]
      (doseq [root ir2-roots]
        (walk root nil :root nil)))
    @ctxs))

;; ── 处理单个 lambda ────────────────────────
(defn- process-lambda [ctx ir2-roots config]
  (let [lam (:lambda ctx)
        parent (:parent ctx)
        role (:role ctx)]
    (case role
      :fn   ;; 直接调用点，内联
      (let [call-node parent
            inlined (lift/inline-call call-node lam config)]
        (mapv #(replace-node % call-node inlined) ir2-roots))
      :let-val   ;; let 绑定值，内联该绑定
      (let [let-node parent
            new-let (inline-let-binding let-node)]
        (mapv #(replace-node % let-node new-let) ir2-roots))
      :args   ;; 作为参数传递（需单态化，暂不处理）
      ir2-roots
      ;; 其他角色（如 :child, :lambda-body 等）忽略
      ir2-roots)))

;; ── 主迭代 ────────────────────────────────
(defn elaborate [ir2-roots config]
  (let [max-iter (cfg/max-iterations config)]
    (loop [roots ir2-roots iter 0]
      (let [ctxs (lambda-contexts roots)]
        (if (empty? ctxs)
          roots
          (if (>= iter max-iter)
            (if (cfg/strict-mode? config)
              (throw (ex-info "Unable to eliminate all closures" {:contexts ctxs}))
              roots)
            (let [new-roots (reduce (fn [acc ctx] (process-lambda ctx acc config)) roots ctxs)]
              (recur new-roots (inc iter)))))))))