(ns top.kzre.homunculus.core.types.elaborate.core
  "IR2 闭包消除 pass。基于多重方法递归遍历，消除 :lambda 节点。"
  (:require
   [clojure.set :as set]
   [top.kzre.homunculus.core.ir2.model :as m]
   [top.kzre.homunculus.core.ir2.protocol :as ir2p]
   [top.kzre.homunculus.core.types.alpha-rename :as alpha]
   [top.kzre.homunculus.core.types.elaborate.protocol :as cfg]
   [top.kzre.homunculus.core.types.free-vars :as free-vars]
   [top.kzre.homunculus.core.types.subst :as subst]
   [top.kzre.homunculus.core.types.utils :as u]))

;; ── 自由变量分析 ──
(defn free-vars-of-lambda [lam]
  (let [bound (set (map :name (:params lam)))]
    (set/difference (free-vars/analyze (:body lam)) bound)))

;; ── let 绑定内联（仅处理 lambda 值） ──
(defn inline-let-binding [let-node]
  (let [bindings (:bindings let-node)
        body (:body let-node)]
    (loop [remaining bindings
           new-bindings []
           new-body body]
      (if-let [[var val] (first remaining)]
        (if (and (satisfies? ir2p/INode val)
                 (= (ir2p/kind val) :lambda))
          (let [renamed-lam (alpha/rename val)
                new-body' (subst/inline-expr new-body (:name var) renamed-lam)]
            (recur (rest remaining) new-bindings new-body'))
          (recur (rest remaining) (conj new-bindings [var val]) new-body))
        (if (seq new-bindings)
          (m/->LetNode (vec new-bindings) new-body (:attrs let-node) (:meta let-node) (:parent let-node))
          new-body)))))

;; ── 单态化：将传递给已知函数的 lambda 特化 ──
(defn monomorphize [call-node target-fn-name idx lam ir2-roots config]
  (let [target-define (u/find-toplevel-define ir2-roots target-fn-name)
        _ (when-not target-define
            (throw (ex-info "Target function not found" {:name target-fn-name})))
        fv (free-vars-of-lambda lam)
        lifted-name (u/fresh-name (or (:fn-name lam) 'closure))
        ;; 提升 lambda
        new-params (into (:params lam)
                         (mapv #(m/->VariableNode % nil nil nil) fv))
        lifted-lam (assoc lam :params new-params)
        lifted-define (m/->DefineNode lifted-name lifted-lam nil nil nil nil)
        ;; 目标函数体替换
        target-lambda (:val target-define)
        target-params (:params target-lambda)
        formal-param (nth target-params idx)
        replace-param (fn replace-param [node]
                        (if (and (satisfies? ir2p/INode node)
                                 (= (ir2p/kind node) :variable)
                                 (= (:name node) (:name formal-param)))
                          (m/->VariableNode (name lifted-name) nil nil nil)
                          (case (ir2p/kind node)
                            :call
                            (m/->CallNode (replace-param (:fn node))
                                          (mapv replace-param (:args node))
                                          (:attrs node) (:meta node) (:parent node))
                            :let
                            (m/->LetNode (mapv (fn [[v e]] [(replace-param v) (replace-param e)])
                                               (:bindings node))
                                         (replace-param (:body node))
                                         (:attrs node) (:meta node) (:parent node))
                            :block
                            (m/->BlockNode (mapv replace-param (:exprs node))
                                           (:attrs node) (:meta node) (:parent node))
                            :if
                            (m/->IfNode (replace-param (:test node))
                                        (replace-param (:then node))
                                        (when (:else node) (replace-param (:else node)))
                                        (:attrs node) (:meta node) (:parent node))
                            :loop
                            (m/->LoopNode (mapv (fn [[v e]] [(replace-param v) (replace-param e)])
                                                (:bindings node))
                                          (replace-param (:body node))
                                          (:attrs node) (:meta node) (:parent node))
                            :while
                            (m/->WhileNode (replace-param (:test node))
                                           (replace-param (:body node))
                                           (:attrs node) (:meta node) (:parent node))
                            :try
                            (m/->TryNode (mapv replace-param (:body node))
                                         (mapv replace-param (:catches node))
                                         (when (:finally node) (mapv replace-param (:finally node)))
                                         (:attrs node) (:meta node) (:parent node))
                            :catch
                            (m/->CatchNode (:class node) (:sym node)
                                           (mapv replace-param (:body node))
                                           (:attrs node) (:meta node) (:parent node))
                            :throw
                            (m/->ThrowNode (replace-param (:expr node))
                                           (:attrs node) (:meta node) (:parent node))
                            :assign
                            (m/->AssignNode (replace-param (:var node))
                                            (replace-param (:val node))
                                            (:attrs node) (:meta node) (:parent node))
                            :vector
                            (m/->VectorNode (mapv replace-param (:items node))
                                            (:attrs node) (:meta node) (:parent node))
                            :map
                            (m/->MapNode (mapv replace-param (:kvs node))
                                         (:attrs node) (:meta node) (:parent node))
                            :lambda node
                            :define node
                            :literal node
                            :variable node
                            (throw (ex-info (str "Unknown node in monomorphize: " (ir2p/kind node))
                                            {:node node})))))
        cloned-body (replace-param (:body target-lambda))
        ;; 特化函数：参数 = 原目标参数 + 自由变量
        specialized-params (into target-params
                                 (mapv #(m/->VariableNode % nil nil nil) fv))
        specialized-lambda (m/->LambdaNode specialized-params cloned-body nil nil nil nil nil)
        specialized-name (u/fresh-name target-fn-name)
        specialized-define (m/->DefineNode specialized-name specialized-lambda nil nil nil nil)
        ;; 新调用点
        old-args (:args call-node)
        new-fn-arg (m/->VariableNode (name lifted-name) nil nil nil)
        fv-args (mapv #(m/->VariableNode % nil nil nil) fv)
        new-args (vec (concat (subvec old-args 0 idx)
                              [new-fn-arg]
                              fv-args
                              (subvec old-args (inc idx))))
        new-call (m/->CallNode (m/->VariableNode (name specialized-name) nil nil nil)
                               new-args
                               (:attrs call-node) (:meta call-node) (:parent call-node))]
    {:new-call new-call
     :new-defines [lifted-define specialized-define]}))

;; ── 多方法分派 ──
(defmulti eliminate
          "消除节点及其子树中的 :lambda。返回更新后的节点。"
          (fn [node _ir2-roots _config _new-defs] (ir2p/kind node)))

(defn has-lambda? [node]
  (if (satisfies? ir2p/INode node)
    (case (ir2p/kind node)
      :define false   ; 允许顶层 define 内包含 lambda
      :lambda true
      (some has-lambda? (ir2p/children node)))
    false))

(defn elaborate
  [ir2-roots config]
  (let [max-iter (cfg/max-iterations config)
        strict?  (cfg/strict-mode? config)]
    (loop [roots ir2-roots iter 0]
      (let [new-defs (atom [])
            new-roots (mapv #(eliminate % roots config new-defs) roots)
            all-roots (into new-roots @new-defs)]
        (if (some has-lambda? all-roots)
          (if (>= iter max-iter)
            (if strict?
              (throw (ex-info "Unable to eliminate all closures" {}))
              all-roots)
            (recur all-roots (inc iter)))
          all-roots)))))