(ns top.kzre.homunculus.core.types.annotate-meta
  "从 node-meta 中提取类型标注，设置到 attrs :type 中。
   使用已知类型集合避免将语义关键字误判为类型。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.model :as t]))

(defn- meta->type
  "从 node-meta 中查找第一个在 known-types 中的关键字，返回对应的 TCon。"
  [meta known-types]
  (when (map? meta)
    (some (fn [k]
            (when (and (keyword? k) (contains? known-types k))
              (t/->TCon k)))
          (keys meta))))

(defmulti annotate-node
          "递归遍历节点，为未指定类型的 VariableNode 添加类型。"
          (fn [node known-types] (ir2p/kind node)))

(defmethod annotate-node :variable [node known-types]
  (if (get-in node [:attrs :type])
    node
    (if-let [ty (meta->type (ir2p/node-meta node) known-types)]
      (m/->VariableNode (:name node) (assoc (:attrs node) :type ty) (:meta node) (:parent node))
      node)))

(defmethod annotate-node :call [node known-types]
  (m/->CallNode (annotate-node (:fn node) known-types)
                (mapv #(annotate-node % known-types) (:args node))
                (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :let [node known-types]
  (m/->LetNode (mapv (fn [[v e]] [(annotate-node v known-types) (annotate-node e known-types)]) (:bindings node))
               (annotate-node (:body node) known-types)
               (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :lambda [node known-types]
  (m/->LambdaNode (mapv #(annotate-node % known-types) (:params node))
                  (annotate-node (:body node) known-types)
                  (:captures node) (:fn-name node)
                  (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :block [node known-types]
  (m/->BlockNode (mapv #(annotate-node % known-types) (:exprs node))
                 (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :if [node known-types]
  (m/->IfNode (annotate-node (:test node) known-types)
              (annotate-node (:then node) known-types)
              (when (:else node) (annotate-node (:else node) known-types))
              (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :while [node known-types]
  (m/->WhileNode (annotate-node (:test node) known-types)
                 (annotate-node (:body node) known-types)
                 (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :assign [node known-types]
  (m/->AssignNode (annotate-node (:var node) known-types)
                  (annotate-node (:val node) known-types)
                  (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :define [node known-types]
  (m/->DefineNode (:name node) (annotate-node (:val node) known-types)
                  (:doc node) (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :try [node known-types]
  (m/->TryNode (mapv #(annotate-node % known-types) (:body node))
               (mapv #(annotate-node % known-types) (:catches node))
               (when (:finally node) (mapv #(annotate-node % known-types) (:finally node)))
               (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :catch [node known-types]
  (m/->CatchNode (:class node) (:sym node)
                 (mapv #(annotate-node % known-types) (:body node))
                 (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :throw [node known-types]
  (m/->ThrowNode (annotate-node (:expr node) known-types)
                 (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :vector [node known-types]
  (m/->VectorNode (mapv #(annotate-node % known-types) (:items node))
                  (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :map [node known-types]
  (m/->MapNode (mapv #(annotate-node % known-types) (:kvs node))
               (:attrs node) (:meta node) (:parent node)))

(defmethod annotate-node :literal [node _] node)
(defmethod annotate-node :default [node _]
  (throw (ex-info (str "Unknown node kind in annotate-meta: " (ir2p/kind node)) {:node node})))

(defn annotate [ir2-roots known-types]
  (mapv #(annotate-node % known-types) ir2-roots))