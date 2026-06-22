(ns top.kzre.homunculus.core.types.ho-elim.analyze
  "高阶分析：遍历 IR2 树，标记高阶函数定义。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]))

(defn high-order?
  "若 lambda 的某个参数在其函数体内被用作调用目标，则返回 true。"
  [lam]
  (let [param-names (set (map n/var-name (n/lambda-params lam)))]
    (letfn [(walk [node]
              (cond
                (n/call-node? node)
                (let [fn-node (n/call-fn node)]
                  (or (and (n/variable-node? fn-node)
                           (contains? param-names (n/var-name fn-node)))
                      (some walk (n/children node))))
                :else
                (some walk (n/children node))))]
      (boolean (walk (n/lambda-body lam))))))


;; ── 多方法：遍历并标记高阶函数 ─────────────────
(defmulti walk (fn [node] (n/kind node)))

(defmethod walk :define [node]
  (if-let [val (n/define-val node)]
    (if (n/lambda-node? val)
      (let [new-val (walk val)
            ho?    (high-order? new-val)]
        (n/make-define (n/define-name node)
                       new-val
                       (n/define-doc node)
                       (if ho?
                         (assoc (n/attrs node) :ho? true)
                         (n/attrs node))
                       (n/node-meta node)
                       (n/parent node)))
      (n/make-define (n/define-name node)
                     (walk val)
                     (n/define-doc node)
                     (n/attrs node)
                     (n/node-meta node)
                     (n/parent node)))
    node))

(defmethod walk :lambda [node]
  (n/make-lambda (mapv walk (n/lambda-params node))
                 (walk (n/lambda-body node))
                 (n/lambda-captures node)
                 (n/lambda-fn-name node)
                 (n/attrs node)
                 (n/node-meta node)
                 (n/parent node)))

(defmethod walk :call [node]
  (n/make-call (walk (n/call-fn node))
               (mapv walk (n/call-args node))
               (n/attrs node)
               (n/node-meta node)
               (n/parent node)))

(defmethod walk :if [node]
  (n/make-if (walk (n/if-test node))
             (walk (n/if-then node))
             (when-let [e (n/if-else node)] (walk e))
             (n/attrs node)
             (n/node-meta node)
             (n/parent node)))

(defmethod walk :block [node]
  (n/make-block (mapv walk (n/block-exprs node))
                (n/attrs node)
                (n/node-meta node)
                (n/parent node)))

(defmethod walk :let [node]
  (let [new-bindings (mapv (fn [[v e]] [(walk v) (walk e)])
                           (n/let-bindings node))
        new-body     (walk (n/let-body node))]
    (n/make-let new-bindings new-body
                (n/attrs node)
                (n/node-meta node)
                (n/parent node))))

(defmethod walk :loop [node]
  (let [new-bindings (mapv (fn [[v e]] [(walk v) (walk e)])
                           (n/loop-bindings node))
        new-body     (walk (n/loop-body node))]
    (n/make-loop new-bindings new-body
                 (n/attrs node)
                 (n/node-meta node)
                 (n/parent node))))

(defmethod walk :while [node]
  (n/make-while (walk (n/while-test node))
                (walk (n/while-body node))
                (n/attrs node)
                (n/node-meta node)
                (n/parent node)))

(defmethod walk :vector [node]
  (n/make-vector (mapv walk (n/vector-items node))
                 (n/attrs node)
                 (n/node-meta node)
                 (n/parent node)))

(defmethod walk :assign [node]
  (n/make-assign (walk (n/assign-var node))
                 (walk (n/assign-val node))
                 (n/attrs node)
                 (n/node-meta node)
                 (n/parent node)))

;; 数组节点
(defmethod walk :new-array [node]
  (n/make-new-array (walk (n/new-array-size node))
                    (n/node-meta node)
                    (n/parent node)))
(defmethod walk :aget [node]
  (n/make-aget (walk (n/aget-target node))
               (walk (n/aget-idx node))
               (n/node-meta node)
               (n/parent node)))
(defmethod walk :aset [node]
  (n/make-aset (walk (n/aset-target node))
               (walk (n/aset-idx node))
               (walk (n/aset-val node))
               (n/node-meta node)
               (n/parent node)))
(defmethod walk :alength [node]
  (n/make-alength (walk (n/alength-target node))
                  (n/node-meta node)
                  (n/parent node)))

(defmethod walk :default [node] node)

;; ── 对外入口 ─────────────────────────────────
(defn analyze
  "递归分析单个 IR2 节点（或根列表），标记所有高阶 define。
   若传入向量，则对每个元素应用 mark-ho。"
  [ir2-node]
  (if (sequential? ir2-node)
    (mapv walk ir2-node)
    (walk ir2-node)))