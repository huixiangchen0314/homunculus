(ns top.kzre.homunculus.core.types.lambda-inline.core
  "Lambda 内联 Pass：消除 let 绑定的局部 lambda。"
  (:require
    [clojure.walk :as walk]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.lambda-inline.protocol :as p]
    [top.kzre.homunculus.core.types.free-vars :as free-vars]
    [top.kzre.homunculus.core.types.subst.api :as subst]))

;; ── 检查变量是否在非调用位置被引用 ──────
(defn has-non-call-usage?
  "检查 body 中是否存在对 var-name 的非调用位置引用。
   例如作为函数参数、返回值、赋值等。"
  [body var-name]
  (let [found (atom false)]
    (walk/prewalk
      (fn [node]
        (when (and (satisfies? ir2p/INode node)
                   (= (n/kind node) :variable)
                   (= (n/var-name node) var-name))
          ;; 判断父节点是否 :call 且该变量是否位于函数位置
          (let [p (n/parent node)]
            (when (or (nil? p)
                      (not= (n/kind p) :call)
                      (not= (n/call-fn p) node))
              (reset! found true))))
        node)
      body)
    @found))

;; ── 收集所有调用点 ──────────────────────
(defn- collect-call-sites [body var-name]
  (let [sites (atom [])]
    (walk/prewalk
      (fn [node]
        (when (and (satisfies? ir2p/INode node)
                   (= (n/kind node) :call))
          (let [fn-node (n/call-fn node)]
            (when (and (= (n/kind fn-node) :variable)
                       (= (n/var-name fn-node) var-name))
              (swap! sites conj node))))
        node)
      body)
    @sites))

;; ── 内联条件 ────────────────────────────
(defn- inline-candidate? [lam config]
  (and (p/should-inline? config lam nil)
       (let [size (count (tree-seq coll? seq (n/lambda-body lam)))]
         (<= size (p/max-inline-size? config)))))

;; ── 内联单个调用点 ──────────────────────
(defn- inline-call-site [call-node lambda-node]
  (subst/inline-call call-node lambda-node nil))

;; ── 替换 body 中所有对 var-name 的调用 ──
(defn- replace-call-sites [body var-name lambda-node]
  (let [sites (collect-call-sites body var-name)]
    (reduce (fn [cur-body site]
              (let [inlined (inline-call-site site lambda-node)]
                (walk/prewalk-replace {site inlined} cur-body)))
            body sites)))

;; ── 内联 let 绑定 ───────────────────────
(defn inline-let
  "如果 let 绑定的 lambda 满足条件，将其内联到 body 中所有调用点。"
  [let-node config]
  (let [bindings (n/let-bindings let-node)
        body     (n/let-body let-node)]
    (loop [remaining bindings
           new-bindings []
           current-body body]
      (if-let [[var val] (first remaining)]
        (if (and (= (n/kind val) :lambda)
                 (empty? (free-vars/free-vars-of-lambda val))
                 (inline-candidate? val config)
                 (not (has-non-call-usage? current-body (n/var-name var))))
          ;; 执行内联
          (let [new-body (replace-call-sites current-body (n/var-name var) val)]
            (recur (rest remaining) new-bindings new-body))
          ;; 保留绑定
          (recur (rest remaining) (conj new-bindings [var val]) current-body))
        (n/make-let new-bindings current-body
                    (n/attrs let-node) (n/node-meta let-node) (n/parent let-node))))))

;; ── 多方法遍历 ──────────────────────────
(defmulti eliminate-inline
          (fn [node _config] (n/kind node)))

(defmethod eliminate-inline :default [node _]
  node)

;; ── 对外入口 ────────────────────────────
(defn inline-pass [ir2-roots config]
  (mapv #(eliminate-inline % config) ir2-roots))