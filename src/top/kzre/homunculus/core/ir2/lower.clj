(ns top.kzre.homunculus.core.ir2.lower
  "IR1 → IR2 降级调度。将新的 IR1 AST 转换为新的 IR2 AST。
   IR2 与 IR1 保持同构，lower-ast 返回单个 IR2 节点（或 nil）。"
  (:require
    [top.kzre.homunculus.core.ir1.ast :as ir1]
    [top.kzre.homunculus.core.ir2.ast :as ir2]))

(defrecord Env [])
(defn make-env [] (->Env))

;; ═══════════════════════════════════════════════
;; 多方法
;; ═══════════════════════════════════════════════
(defmulti lower-ast
          "将 IR1 节点降低为单个 IR2 节点，返回 nil 表示该节点在 IR2 中消失。"
          (fn [ir1-node _env] (ir1/kind ir1-node)))

(defmethod lower-ast :default [_node _env] nil)

;; ═══════════════════════════════════════════════
;; 入口函数
;; ═══════════════════════════════════════════════
(defn lower-node
  "降级单个 IR1 节点，返回 IR2 节点（可能为 nil）。"
  [ir1-node & [env]]
  (let [env (or env (make-env))]
    (lower-ast ir1-node env)))

(defn lower-nodes
  "降级 IR1 节点序列，返回 IR2 节点序列（过滤 nil）。"
  [ir1-nodes & [env]]
  (let [env (or env (make-env))]
    (keep #(lower-ast % env) ir1-nodes)))

(defn lower
  "对外入口：降级 IR1 根节点序列为 IR2 节点序列。"
  [ir1-nodes]
  (lower-nodes ir1-nodes (make-env)))

;; ═══════════════════════════════════════════════
;; 基础节点
;; ═══════════════════════════════════════════════
(defmethod lower-ast :literal [node _env]
  (ir2/->Literal (:val node) {} (ir1/node-meta node)))

(defmethod lower-ast :symbol [node _env]
  (ir2/->Variable (:name node) {} (ir1/node-meta node)))

(defmethod lower-ast :keyword [node _env]
  (let [ns (:ns node)
        n  (:name node)
        kw (if ns (keyword ns n) (keyword n))]
    (ir2/->Literal kw {} (ir1/node-meta node))))

;; ═══════════════════════════════════════════════
;; 辅助结构节点（显式递归，保持同构）
;; ═══════════════════════════════════════════════
(defmethod lower-ast :pair [node env]
  (ir2/->Pair (lower-node (:key node) env)
              (lower-node (:val node) env)
              {} (ir1/node-meta node)))

(defmethod lower-ast :binding [node env]
  (ir2/->Binding (lower-node (:var node) env)
                 (lower-node (:val node) env)
                 {} (ir1/node-meta node)))

(defmethod lower-ast :param [node env]
  (ir2/->Param (lower-node (:name node) env)
               {} (ir1/node-meta node)))

(defmethod lower-ast :catch [node env]
  (ir2/->Catch (:class node) (:sym node)
               (mapv #(lower-node % env) (:body node))
               {} (ir1/node-meta node)))

(defmethod lower-ast :record-field [node env]
  (ir2/->RecordField (:name node)
                     (some-> (:init node) (lower-node env))
                     {} (ir1/node-meta node)))

(defmethod lower-ast :protocol-impl [node env]
  (ir2/->ProtocolImpl (lower-node (:protocol-sym node) env)
                      (mapv #(lower-node % env) (:methods node))
                      {} (ir1/node-meta node)))

(defmethod lower-ast :protocol-method [node env]
  (ir2/->ProtocolMethod (:name node)
                        (mapv #(lower-node % env) (:params node))
                        (lower-node (:body node) env)
                        {} (ir1/node-meta node)))

;; ═══════════════════════════════════════════════
;; 集合与容器
;; ═══════════════════════════════════════════════
(defmethod lower-ast :vector [node env]
  (ir2/->Vector (mapv #(lower-node % env) (:items node))
                {} (ir1/node-meta node)))

(defmethod lower-ast :map [node env]
  (ir2/->Map (mapv #(lower-node % env) (:pairs node))
             {} (ir1/node-meta node)))

;; ═══════════════════════════════════════════════
;; 控制流与作用域
;; ═══════════════════════════════════════════════
(defmethod lower-ast :do [node env]
  (ir2/->Block (mapv #(lower-node % env) (:exprs node))
               {} (ir1/node-meta node)))

(defmethod lower-ast :if [node env]
  (ir2/->If (lower-node (:test node) env)
            (lower-node (:then node) env)
            (some-> (:else node) (lower-node env))
            {} (ir1/node-meta node)))

(defmethod lower-ast :let [node env]
  (ir2/->Let (mapv #(lower-node % env) (:bindings node))
             (lower-node (:body node) env)
             {} (ir1/node-meta node)))

(defmethod lower-ast :loop [node env]
  (ir2/->Loop (mapv #(lower-node % env) (:bindings node))
              (lower-node (:body node) env)
              {} (ir1/node-meta node)))

(defmethod lower-ast :recur [node env]
  (ir2/->Recur (mapv #(lower-node % env) (:exprs node))
               {} (ir1/node-meta node)))

(defmethod lower-ast :fn [node env]
  (let [name-node (some-> (:name node) (lower-node env))
        params    (mapv #(lower-node % env) (:params node))
        body      (lower-node (:body node) env)]
    (ir2/->Lambda name-node params body {} (ir1/node-meta node))))

;; ═══════════════════════════════════════════════
;; 副作用与异常
;; ═══════════════════════════════════════════════
(defmethod lower-ast :throw [node env]
  (ir2/->Throw (lower-node (:expr node) env) {} (ir1/node-meta node)))

(defmethod lower-ast :set [node env]
  (ir2/->Assign (lower-node (:var node) env)
                (lower-node (:val node) env)
                {} (ir1/node-meta node)))

(defmethod lower-ast :try [node env]
  (ir2/->Try (lower-node (:body node) env)
             (mapv #(lower-node % env) (:catches node))
             (some-> (:finally node) (lower-node env))
             {} (ir1/node-meta node)))

;; ═══════════════════════════════════════════════
;; 调用与原语
;; ═══════════════════════════════════════════════
(def ^:private primitive-ops
  '#{%%aget %%aset %%new-array %%alength})

(defmethod lower-ast :call [node env]
  (let [fn-node (lower-node (:op node) env)
        args    (mapv #(lower-node % env) (:args node))]
    (if (and (= :variable (ir2/kind fn-node))
             (contains? primitive-ops (:name fn-node)))
      (case (:name fn-node)
        %%aget      (ir2/->Aget (first args) (second args) {} (ir1/node-meta node))
        %%aset      (ir2/->Aset (first args) (second args) (nth args 2) {} (ir1/node-meta node))
        %%new-array (ir2/->NewArray (first args) {} (ir1/node-meta node))
        %%alength   (ir2/->Alength (first args) {} (ir1/node-meta node)))
      (ir2/->Call fn-node args {} (ir1/node-meta node)))))

;; ═══════════════════════════════════════════════
;; 定义与声明
;; ═══════════════════════════════════════════════
(defmethod lower-ast :def [node env]
  (let [name-sym (-> node :name :name)
        val      (some-> (:val node) (lower-node env))
        doc      (:doc node)
        meta     (ir1/node-meta node)]    ;; 已包含原 attr-map 内容
    (ir2/->Define name-sym doc val {} meta)))

(defmethod lower-ast :ns [node _env]
  (ir2/->Ns (:name node) (:doc node)
            (:requires node) {} (ir1/node-meta node)))

;; ═══════════════════════════════════════════════
;; 记录与协议（保持同构）
;; ═══════════════════════════════════════════════
(defmethod lower-ast :record [node env]
  (ir2/->Record (:name node)
                (mapv #(lower-node % env) (:fields node))
                (mapv #(lower-node % env) (:protocols node))
                {} (ir1/node-meta node)))

(defmethod lower-ast :protocol [node _env]
  (ir2/->Protocol (:name node) (:funcs node) {} (ir1/node-meta node)))

;; ═══════════════════════════════════════════════
;; 成员访问
;; ═══════════════════════════════════════════════
(defmethod lower-ast :member-access [node env]
  (ir2/->MemberAccess (lower-node (:target node) env)
                      (:accessor node)
                      (mapv #(lower-node % env) (:args node))
                      {} (ir1/node-meta node)))

;; ═══════════════════════════════════════════════
;; 特殊形式消除（仍返回单节点）
;; ═══════════════════════════════════════════════
(defmethod lower-ast :quote [node env]
  (lower-ast (:expr node) env))   ;; quote 消失，直接降低内部

(defmethod lower-ast :var [node env]
  (lower-ast (:var-sym node) env)) ;; var 消失，返回变量的降低结果