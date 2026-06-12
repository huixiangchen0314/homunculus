(ns top.kzre.homunculus.core.types.recur-elim.core
  "消除 loop-recur 递归，将 LoopNode 转换为 WhileNode。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.utils :as u]))

(defmulti eliminate
          (fn [node] (ir2p/kind node)))

(defn transform-loop [loop-node]
  (let [bindings (:bindings loop-node)
        body     (:body loop-node)
        var-names (mapv (comp :name first) bindings)
        result-var (u/fresh-name 'result)
        recur-flag (u/fresh-name 'recur?)

        convert-expr (fn convert-expr [node]
                       (if (satisfies? ir2p/INode node)
                         (case (ir2p/kind node)
                           :recur
                           (throw (ex-info "recur used outside tail position" {:node node}))
                           :literal
                           node
                           :variable
                           node
                           :if
                           (m/->IfNode (convert-expr (:test node))
                                       (convert-expr (:then node))
                                       (convert-expr (:else node))
                                       (:attrs node) (:meta node) (:parent node))
                           :let
                           (m/->LetNode
                             (mapv (fn [[v e]] [(convert-expr v) (convert-expr e)]) (:bindings node))
                             (convert-expr (:body node))
                             (:attrs node) (:meta node) (:parent node))
                           :block
                           (m/->BlockNode (mapv convert-expr (:exprs node))
                                          (:attrs node) (:meta node) (:parent node))
                           :call
                           (m/->CallNode (convert-expr (:fn node))
                                         (mapv convert-expr (:args node))
                                         (:attrs node) (:meta node) (:parent node))
                           :assign
                           (m/->AssignNode (convert-expr (:var node))
                                           (convert-expr (:val node))
                                           (:attrs node) (:meta node) (:parent node))
                           ;; 其他节点类型原样返回（通常不会出现在非尾位置）
                           node)
                         node))

        convert-tail (fn convert-tail [node]
                       (if (satisfies? ir2p/INode node)
                         (case (ir2p/kind node)
                           :recur
                           (let [args (:args node)
                                 assigns (mapv (fn [var arg]
                                                 (m/->AssignNode
                                                   (m/->VariableNode var nil nil nil)
                                                   (convert-expr arg)
                                                   nil nil nil))
                                               var-names args)
                                 set-flag (m/->AssignNode
                                            (m/->VariableNode recur-flag nil nil nil)
                                            (m/->LiteralNode true nil nil nil)
                                            nil nil nil)]
                             (m/->BlockNode (into assigns [set-flag]) nil nil nil))
                           :if
                           (m/->IfNode (convert-expr (:test node))
                                       (convert-tail (:then node))
                                       (convert-tail (:else node))
                                       (:attrs node) (:meta node) (:parent node))
                           :block
                           (let [exprs (:exprs node)
                                 butlast (butlast exprs)
                                 last-expr (last exprs)
                                 converted-butlast (mapv convert-expr butlast)
                                 converted-last (convert-tail last-expr)]
                             (m/->BlockNode (into converted-butlast [converted-last])
                                            (:attrs node) (:meta node) (:parent node)))
                           :let
                           (m/->LetNode
                             (mapv (fn [[v e]] [(convert-expr v) (convert-expr e)]) (:bindings node))
                             (convert-tail (:body node))
                             (:attrs node) (:meta node) (:parent node))
                           ;; 其他表达式视为正常返回值
                           (let [result-assign (m/->AssignNode
                                                 (m/->VariableNode result-var nil nil nil)
                                                 node
                                                 nil nil nil)
                                 flag-assign (m/->AssignNode
                                               (m/->VariableNode recur-flag nil nil nil)
                                               (m/->LiteralNode false nil nil nil)
                                               nil nil nil)]
                             (m/->BlockNode [result-assign flag-assign] nil nil nil)))
                         (let [result-assign (m/->AssignNode
                                               (m/->VariableNode result-var nil nil nil)
                                               node
                                               nil nil nil)
                               flag-assign (m/->AssignNode
                                             (m/->VariableNode recur-flag nil nil nil)
                                             (m/->LiteralNode false nil nil nil)
                                             nil nil nil)]
                           (m/->BlockNode [result-assign flag-assign] nil nil nil))))]

    (let [loop-var-bindings (mapv (fn [[var init]]
                                    [(m/->VariableNode (:name var) nil nil nil)
                                     (convert-expr init)])
                                  bindings)
          all-bindings (into loop-var-bindings
                             [[(m/->VariableNode result-var nil nil nil) (m/->LiteralNode nil nil nil nil)]
                              [(m/->VariableNode recur-flag nil nil nil) (m/->LiteralNode true nil nil nil)]])
          loop-body (convert-tail body)
          while-node (m/->WhileNode
                       (m/->VariableNode recur-flag nil nil nil)
                       loop-body
                       nil nil nil)
          let-body (m/->BlockNode [while-node (m/->VariableNode result-var nil nil nil)]
                                  nil nil nil)]
      (m/->LetNode all-bindings let-body nil nil nil))))