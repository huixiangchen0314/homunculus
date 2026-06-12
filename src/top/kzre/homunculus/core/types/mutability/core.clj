(ns top.kzre.homunculus.core.types.mutability.core
  "可变性分析：找出所有被 :assign 赋值的 :variable 节点，在 :attrs 中标记 {:mutable true}。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmulti annotate
          "递归遍历，标记可变变量。返回更新后的节点。"
          (fn [node mutable-vars] (ir2p/kind node)))

;; ── 叶子 ──
(defmethod annotate :literal [node _] node)

(defmethod annotate :variable [node mutable-vars]
  (if (contains? mutable-vars (:name node))
    (m/->VariableNode (:name node)
                      (assoc (:attrs node) :mutable true)
                      (:meta node)
                      (:parent node))
    node))

;; ── 赋值 ──
(defmethod annotate :assign [node mutable-vars]
  (let [var-name (:name (:var node))
        new-mutable (conj mutable-vars var-name)
        new-var (annotate (:var node) new-mutable)
        new-val (annotate (:val node) new-mutable)]
    (m/->AssignNode new-var new-val (:attrs node) (:meta node) (:parent node))))

;; ── 复合节点 ──
(defmethod annotate :call [node mutable-vars]
  (m/->CallNode (annotate (:fn node) mutable-vars)
                (mapv #(annotate % mutable-vars) (:args node))
                (:attrs node) (:meta node) (:parent node)))

(defmethod annotate :let [node mutable-vars]
  (let [new-bindings (mapv (fn [[v e]]
                             [(annotate v mutable-vars)
                              (annotate e mutable-vars)])
                           (:bindings node))
        new-body (annotate (:body node) mutable-vars)]
    (m/->LetNode new-bindings new-body (:attrs node) (:meta node) (:parent node))))

(defmethod annotate :lambda [node mutable-vars]
  (m/->LambdaNode (mapv #(annotate % mutable-vars) (:params node))
                  (annotate (:body node) mutable-vars)
                  (:captures node) (:fn-name node)
                  (:attrs node) (:meta node) (:parent node)))

(defmethod annotate :block [node mutable-vars]
  (m/->BlockNode (mapv #(annotate % mutable-vars) (:exprs node))
                 (:attrs node) (:meta node) (:parent node)))

(defmethod annotate :if [node mutable-vars]
  (m/->IfNode (annotate (:test node) mutable-vars)
              (annotate (:then node) mutable-vars)
              (when (:else node) (annotate (:else node) mutable-vars))
              (:attrs node) (:meta node) (:parent node)))

(defmethod annotate :loop [node mutable-vars]
  (let [new-bindings (mapv (fn [[v e]]
                             [(annotate v mutable-vars)
                              (annotate e mutable-vars)])
                           (:bindings node))
        new-body (annotate (:body node) mutable-vars)]
    (m/->LoopNode new-bindings new-body (:attrs node) (:meta node) (:parent node))))

(defmethod annotate :define [node mutable-vars]
  (m/->DefineNode (:name node)
                  (annotate (:val node) mutable-vars)
                  (:doc node) (:attrs node) (:meta node) (:parent node)))

(defmethod annotate :while [node mutable-vars]
  (m/->WhileNode (annotate (:test node) mutable-vars)
                 (annotate (:body node) mutable-vars)
                 (:attrs node) (:meta node) (:parent node)))

(defmethod annotate :try [node mutable-vars]
  (m/->TryNode (mapv #(annotate % mutable-vars) (:body node))
               (mapv #(annotate % mutable-vars) (:catches node))
               (when (:finally node) (mapv #(annotate % mutable-vars) (:finally node)))
               (:attrs node) (:meta node) (:parent node)))

(defmethod annotate :catch [node mutable-vars]
  (m/->CatchNode (:class node) (:sym node)
                 (mapv #(annotate % mutable-vars) (:body node))
                 (:attrs node) (:meta node) (:parent node)))

(defmethod annotate :throw [node mutable-vars]
  (m/->ThrowNode (annotate (:expr node) mutable-vars)
                 (:attrs node) (:meta node) (:parent node)))

(defmethod annotate :vector [node mutable-vars]
  (m/->VectorNode (mapv #(annotate % mutable-vars) (:items node))
                  (:attrs node) (:meta node) (:parent node)))

(defmethod annotate :map [node mutable-vars]
  (m/->MapNode (mapv #(annotate % mutable-vars) (:kvs node))
               (:attrs node) (:meta node) (:parent node)))

(defmethod annotate :default [node _]
  (throw (ex-info (str "Unknown node kind in mutability: " (ir2p/kind node))
                  {:node node})))

;; ── 分析入口 ──
(defn analyze
  "对 IR2 根节点列表进行可变性分析，返回新的 IR2 根列表。"
  [ir2-roots]
  (let [mutable-vars (atom #{})
        collect (fn collect [node]
                  (when (satisfies? ir2p/INode node)
                    (case (ir2p/kind node)
                      :assign (swap! mutable-vars conj (:name (:var node)))
                      nil)
                    (doseq [c (ir2p/children node)] (collect c))))]
    (doseq [root ir2-roots] (collect root))
    (mapv #(annotate % @mutable-vars) ir2-roots)))