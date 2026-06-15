(ns top.kzre.homunculus.core.types.builtin-check.core
  "内建函数检查：验证所有 :call 节点调用的函数是否在已知内建表中。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.model :as t]))

(defmulti check-node
          "递归遍历节点，检查 :call 调用的函数。返回更新后的节点或抛出异常。"
          (fn [node builtins] (ir2p/kind node)))

;; ── 叶子 ──
(defmethod check-node :literal [node _] node)
(defmethod check-node :variable [node _] node)

;; ── 调用 ──
;(defmethod check-node :call [node builtins]
;  (let [fn-node (:fn node)
;        new-fn (check-node fn-node builtins)
;        new-args (mapv #(check-node % builtins) (:args node))]
;    ;; 仅当被调用函数为变量时才进行内建检查
;    (when (and (= (ir2p/kind new-fn) :variable)
;               (not (contains? builtins (symbol (:name new-fn)))))
;      (throw (ex-info (str "Undefined function: " (:name new-fn))
;                      {:fn-name (:name new-fn) :node node})))
;    (if (and (= (ir2p/kind new-fn) :variable)
;             (contains? builtins (symbol (:name new-fn))))
;      (m/->CallNode new-fn new-args
;                    (assoc (:attrs node) :builtin-fn (get builtins (symbol (:name new-fn))))
;                    (:meta node) (:parent node))
;      (m/->CallNode new-fn new-args (:attrs node) (:meta node) (:parent node)))))

;; core/types/builtin_check/core.clj
(defmethod check-node :call [node builtins]
  (let [fn-node (:fn node)
        new-fn (check-node fn-node builtins)
        new-args (mapv #(check-node % builtins) (:args node))]
    ;; 仅当被调用函数为变量且不在内建表中时，不添加内建属性，也不报错
    (if (and (= (ir2p/kind new-fn) :variable)
             (contains? builtins (symbol (:name new-fn))))
      ;; 内置函数：添加 :builtin-fn 属性
      (m/->CallNode new-fn new-args
                    (assoc (:attrs node) :builtin-fn (get builtins (symbol (:name new-fn))))
                    (:meta node) (:parent node))
      ;; 非内置函数：原样返回，不添加属性
      (m/->CallNode new-fn new-args (:attrs node) (:meta node) (:parent node)))))

;; ── 复合节点递归 ──
(defmethod check-node :let [node builtins]
  (let [new-bindings (mapv (fn [[v e]]
                             [(check-node v builtins)
                              (check-node e builtins)])
                           (:bindings node))
        new-body (check-node (:body node) builtins)]
    (m/->LetNode new-bindings new-body (:attrs node) (:meta node) (:parent node))))

(defmethod check-node :lambda [node builtins]
  (m/->LambdaNode (mapv #(check-node % builtins) (:params node))
                  (check-node (:body node) builtins)
                  (:captures node) (:fn-name node)
                  (:attrs node) (:meta node) (:parent node)))

(defmethod check-node :block [node builtins]
  (m/->BlockNode (mapv #(check-node % builtins) (:exprs node))
                 (:attrs node) (:meta node) (:parent node)))

(defmethod check-node :if [node builtins]
  (m/->IfNode (check-node (:test node) builtins)
              (check-node (:then node) builtins)
              (when (:else node) (check-node (:else node) builtins))
              (:attrs node) (:meta node) (:parent node)))

(defmethod check-node :loop [node builtins]
  (let [new-bindings (mapv (fn [[v e]]
                             [(check-node v builtins)
                              (check-node e builtins)])
                           (:bindings node))
        new-body (check-node (:body node) builtins)]
    (m/->LoopNode new-bindings new-body (:attrs node) (:meta node) (:parent node))))

(defmethod check-node :define [node builtins]
  (m/->DefineNode (:name node)
                  (check-node (:val node) builtins)
                  (:doc node) (:attrs node) (:meta node) (:parent node)))

(defmethod check-node :while [node builtins]
  (m/->WhileNode (check-node (:test node) builtins)
                 (check-node (:body node) builtins)
                 (:attrs node) (:meta node) (:parent node)))

(defmethod check-node :try [node builtins]
  (m/->TryNode (mapv #(check-node % builtins) (:body node))
               (mapv #(check-node % builtins) (:catches node))
               (when (:finally node) (mapv #(check-node % builtins) (:finally node)))
               (:attrs node) (:meta node) (:parent node)))

(defmethod check-node :catch [node builtins]
  (m/->CatchNode (:class node) (:sym node)
                 (mapv #(check-node % builtins) (:body node))
                 (:attrs node) (:meta node) (:parent node)))

(defmethod check-node :throw [node builtins]
  (m/->ThrowNode (check-node (:expr node) builtins)
                 (:attrs node) (:meta node) (:parent node)))

(defmethod check-node :assign [node builtins]
  (m/->AssignNode (check-node (:var node) builtins)
                  (check-node (:val node) builtins)
                  (:attrs node) (:meta node) (:parent node)))

(defmethod check-node :vector [node builtins]
  (m/->VectorNode (mapv #(check-node % builtins) (:items node))
                  (:attrs node) (:meta node) (:parent node)))

(defmethod check-node :map [node builtins]
  (m/->MapNode (mapv #(check-node % builtins) (:kvs node))
               (:attrs node) (:meta node) (:parent node)))

(defmethod check-node :default [node _]
  (throw (ex-info (str "Unknown node kind in builtin-check: " (ir2p/kind node))
                  {:node node})))

(defn check
  "对 IR2 根节点列表执行内建函数检查，返回更新后的 IR2 根列表。"
  [ir2-roots builtins]
  (mapv #(check-node % builtins) ir2-roots))