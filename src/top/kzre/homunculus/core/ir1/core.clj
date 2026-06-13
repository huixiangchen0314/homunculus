(ns top.kzre.homunculus.core.ir1.core
  "IR1 核心：基于 defrecord 的 AST 节点构造与表单解析。
   所有特殊形式的解析逻辑在 ir1.forms 中。"
  (:require [top.kzre.homunculus.core.ir1.protocol :as p]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defn attach-parents
  "递归设置 node 及其子节点的 parent 指针，返回全新的树。
   对于非 INode 子节点（如字面量、nil）保持原样。"
  [node parent]
  (if (satisfies? p/INode node)
    (let [with-parent (p/set-parent node parent)
          childs      (p/children with-parent)
          new-children (mapv (fn [c]
                               (if (satisfies? p/INode c)
                                 (attach-parents c with-parent)
                                 c))
                             childs)]
      ;; 重新构造节点，使所有字段保持一致（尤其 parent 更新）
      (case (p/kind with-parent)
        :literal with-parent
        :symbol  with-parent
        :vector  (m/->VectorNode (:items with-parent) (:meta with-parent) (:parent with-parent))
        :map     (m/->MapNode (:pairs with-parent) (:meta with-parent) (:parent with-parent))
        :call    (m/->CallNode (:op with-parent) (:args with-parent) (:meta with-parent) (:parent with-parent))
        :if      (m/->IfNode (:test with-parent) (:then with-parent) (:else with-parent) (:meta with-parent) (:parent with-parent))
        :do      (m/->DoNode (:exprs with-parent) (:meta with-parent) (:parent with-parent))
        :let     (m/->LetNode (:bindings with-parent) (:body with-parent) (:bindings-count with-parent) (:meta with-parent) (:parent with-parent))
        :fn      (m/->FnNode (:name with-parent) (:params with-parent) (:body with-parent) (:meta with-parent) (:parent with-parent))
        :def     (m/->DefNode (:name with-parent) (:doc with-parent) (:attr with-parent) (:val with-parent) (:meta with-parent) (:parent with-parent))
        :loop    (m/->LoopNode (:bindings with-parent) (:body with-parent) (:bindings-count with-parent) (:meta with-parent) (:parent with-parent))
        :recur   (m/->RecurNode (:exprs with-parent) (:meta with-parent) (:parent with-parent))
        :quote   (m/->QuoteNode (:expr with-parent) (:meta with-parent) (:parent with-parent))
        :var     (m/->VarNode (:var-sym with-parent) (:meta with-parent) (:parent with-parent))
        :throw   (m/->ThrowNode (:expr with-parent) (:meta with-parent) (:parent with-parent))
        :set!    (m/->SetNode (:var with-parent) (:val with-parent) (:meta with-parent) (:parent with-parent))
        :try     (m/->TryNode (:body with-parent) (:catches with-parent) (:finally with-parent) (:meta with-parent) (:parent with-parent))
        :catch   (m/->CatchNode (:class with-parent) (:sym with-parent) (:body with-parent) (:meta with-parent) (:parent with-parent))
        with-parent))
    node))

(declare ->ir1)

;; ── 表单 → 节点记录 分派器 ──────────────
(defmulti form->node
          (fn [form]
            (cond
              (or (number? form) (string? form) (true? form) (false? form)
                  (nil? form) (keyword? form) (char? form)) :literal
              (symbol? form) :symbol
              (vector? form) :vector
              (map? form)    :map
              (seq? form)    (if (symbol? (first form)) (first form) :call)
              :else (throw (ex-info (str "Unsupported form: " form) {:form form})))))

(defmethod form->node :literal [form]
  (m/->LiteralNode form nil nil))
(defmethod form->node :symbol [form]
  (m/->SymbolNode form (meta form) nil))

(defmethod form->node :call [form]
  (let [[op & args] form]
    (m/->CallNode op args nil nil)))
(defmethod form->node :default [form]
  (if (seq? form)
    (let [[op & args] form]
      (m/->CallNode op args nil nil))
    (throw (ex-info (str "Unsupported form: " form) {:form form}))))

(defmulti build-tree (fn [node] (p/kind node)))

(defmethod build-tree :literal [node] node)
(defmethod build-tree :symbol  [node] node)


(defmethod build-tree :call [node]
  (m/->CallNode (->ir1 (:op node))
                (mapv ->ir1 (:args node))
                (:meta node)
                (:parent node)))

(defn ->ir1 [form]
  (let [raw-node (form->node form)
        tree     (build-tree raw-node)]
    (attach-parents tree nil)))