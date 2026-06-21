(ns top.kzre.homunculus.core.types.mutability.core
  "可变性分析：找出所有被 :assign 赋值的 :variable 节点，在 :attrs 中标记 {:mutable true}。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

;; ── 多方法：递归遍历并标注可变变量 ──────
(defmulti annotate
          "递归遍历，标记可变变量。返回更新后的节点。"
          (fn [node mutable-vars] (n/kind node)))

;; ── 叶子节点：直接返回（literal 无变化） ──
(defmethod annotate :literal [node _] node)

;; ── 变量节点：若在可变集合中，则添加 {:mutable true} ──
(defmethod annotate :variable [node mutable-vars]
  (if (contains? mutable-vars (n/var-name node))
    (n/make-variable (n/var-name node)
                     (assoc (n/attrs node) :mutable true)
                     (n/node-meta node)
                     (n/parent node))
    node))

;; ── 赋值节点：收集 var 名，然后递归处理子节点 ──
(defmethod annotate :assign [node mutable-vars]
  (let [var-name (n/var-name (n/assign-var node))
        new-mutable (conj mutable-vars var-name)]
    (n/make-assign (annotate (n/assign-var node) new-mutable)
                   (annotate (n/assign-val node) new-mutable)
                   (n/attrs node) (n/node-meta node) (n/parent node))))

;; ── 数组特殊节点 ──
(defmethod annotate :new-array [node mutable-vars]
  (n/make-new-array (annotate (n/new-array-size node) mutable-vars)
                    (n/node-meta node)
                    (n/parent node)))

(defmethod annotate :aget [node mutable-vars]
  (n/make-aget (annotate (n/aget-target node) mutable-vars)
               (annotate (n/aget-idx node) mutable-vars)
               (n/node-meta node)
               (n/parent node)))

(defmethod annotate :aset [node mutable-vars]
  (n/make-aset (annotate (n/aset-target node) mutable-vars)
               (annotate (n/aset-idx node) mutable-vars)
               (annotate (n/aset-val node) mutable-vars)
               (n/node-meta node)
               (n/parent node)))

(defmethod annotate :alength [node mutable-vars]
  (n/make-alength (annotate (n/alength-target node) mutable-vars)
                  (n/node-meta node)
                  (n/parent node)))



;; ── 以下为所有容器节点，统一模式：递归处理子节点并重建 ──

(defmethod annotate :call [node mutable-vars]
  (n/make-call (annotate (n/call-fn node) mutable-vars)
               (mapv #(annotate % mutable-vars) (n/call-args node))
               (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :let [node mutable-vars]
  (let [new-bindings (mapv (fn [[v e]]
                             [(annotate v mutable-vars)
                              (annotate e mutable-vars)])
                           (n/let-bindings node))
        new-body (annotate (n/let-body node) mutable-vars)]
    (n/make-let new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod annotate :lambda [node mutable-vars]
  (n/make-lambda (mapv #(annotate % mutable-vars) (n/lambda-params node))
                 (annotate (n/lambda-body node) mutable-vars)
                 (n/lambda-captures node) (n/lambda-fn-name node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :block [node mutable-vars]
  (n/make-block (mapv #(annotate % mutable-vars) (n/block-exprs node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :if [node mutable-vars]
  (n/make-if (annotate (n/if-test node) mutable-vars)
             (annotate (n/if-then node) mutable-vars)
             (when-let [e (n/if-else node)] (annotate e mutable-vars))
             (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :loop [node mutable-vars]
  (let [new-bindings (mapv (fn [[v e]]
                             [(annotate v mutable-vars)
                              (annotate e mutable-vars)])
                           (n/loop-bindings node))
        new-body (annotate (n/loop-body node) mutable-vars)]
    (n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod annotate :define [node mutable-vars]
  (n/make-define (n/define-name node)
                 (annotate (n/define-val node) mutable-vars)
                 (n/define-doc node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :while [node mutable-vars]
  (n/make-while (annotate (n/while-test node) mutable-vars)
                (annotate (n/while-body node) mutable-vars)
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :try [node mutable-vars]
  (n/make-try (annotate (n/try-body node) mutable-vars)
              (mapv #(annotate % mutable-vars) (n/try-catches node))
              (when-let [f (n/try-finally node)] (annotate f mutable-vars))
              (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :catch [node mutable-vars]
  (n/make-catch (n/catch-class node) (n/catch-sym node)
                (mapv #(annotate % mutable-vars) (n/catch-body node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :throw [node mutable-vars]
  (n/make-throw (annotate (n/throw-expr node) mutable-vars)
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :vector [node mutable-vars]
  (n/make-vector (mapv #(annotate % mutable-vars) (n/vector-items node))
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :map [node mutable-vars]
  (n/make-map (mapv #(annotate % mutable-vars) (n/map-kvs node))
              (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :recur [node mutable-vars]
  (n/make-recur (mapv #(annotate % mutable-vars) (n/recur-args node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :convert [node mutable-vars]
  (n/make-convert (annotate (n/convert-expr node) mutable-vars)
                  (n/convert-src-ty node) (n/convert-dst-ty node) (n/convert-cost node)
                  (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod annotate :member-access [node mutable-vars]
  (n/make-member-access (annotate (n/access-target node) mutable-vars)
                        (n/access-member node)
                        (mapv #(annotate % mutable-vars) (n/access-args node))
                        (n/node-meta node) (n/parent node)))

;; ── 无子节点的声明节点 ──
(defmethod annotate :ns [node _] node)
(defmethod annotate :record [node _] node)
(defmethod annotate :protocol [node _] node)

(defmethod annotate :default [node _] node)

;; ── 分析入口 ──
(defn analyze
  "对 IR2 根节点列表进行可变性分析，返回新的 IR2 根列表。"
  [ir2-roots]
  (let [mutable-vars (atom #{})
        collect (fn collect [node]
                  (when (satisfies? ir2p/INode node)
                    (when (= (n/kind node) :assign)
                      (swap! mutable-vars conj (n/var-name (n/assign-var node))))
                    (doseq [c (n/children node)] (collect c))))]
    (doseq [root ir2-roots] (collect root))
    (mapv #(annotate % @mutable-vars) ir2-roots)))