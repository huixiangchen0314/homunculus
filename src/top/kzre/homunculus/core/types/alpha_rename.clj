(ns top.kzre.homunculus.core.types.alpha-rename
  "Alpha 重命名：为所有局部变量生成唯一名称，避免变量捕获。
   纯函数式实现：通过返回 [new-node, new-table] 传递重命名环境。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.utils :as u]))

;; ── 多方法分派 ──────────────────────────
(defmulti rename-node
          "返回 [new-node, new-table]"
          (fn [node table] (n/kind node)))

;; ── 叶子节点：表不变 ────────────────────
(defmethod rename-node :literal [node table] [node table])
(defmethod rename-node :variable [node table]
  (if-let [new-name (get table (n/var-name node))]
    [(n/make-variable new-name (n/attrs node) (n/node-meta node) (n/parent node)) table]
    [node table]))

;; ── 引入新绑定的节点 ────────────────────
(defmethod rename-node :lambda [node table]
  (let [old-params (n/lambda-params node)
        new-names (mapv (fn [p]
                          (let [old-name (n/var-name p)]
                            [old-name (u/fresh-name old-name)]))
                        old-params)
        table' (reduce (fn [t [old new]] (assoc t old new)) table new-names)
        [new-body table''] (rename-node (n/lambda-body node) table')
        new-params (mapv (fn [p [old new]]
                           (n/make-variable new (n/attrs p) (n/node-meta p) (n/parent p)))
                         old-params new-names)]
    [(n/make-lambda new-params new-body
                    (n/lambda-captures node) (n/lambda-fn-name node)
                    (n/attrs node) (n/node-meta node) (n/parent node))
     table'']))

(defmethod rename-node :let [node table]
  ;; 顺序处理绑定，避免丢失依赖
  (let [old-bindings (n/let-bindings node)
        [new-bindings table1]
        (reduce (fn [[bnds t] [var val]]
                  ;; 先重命名 val（使用当前表）
                  (let [[new-val t1] (rename-node val t)
                        old-name (n/var-name var)
                        new-name (u/fresh-name old-name)
                        t2 (assoc t1 old-name new-name)
                        new-var (n/make-variable new-name (n/attrs var) (n/node-meta var) (n/parent var))]
                    [(conj bnds [new-var new-val]) t2]))
                [[] table]
                old-bindings)
        [new-body table2] (rename-node (n/let-body node) table1)]
    [(n/make-let new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :loop [node table]
  (let [old-bindings (n/loop-bindings node)
        [new-bindings table1]
        (reduce (fn [[bnds t] [var val]]
                  (let [[new-val t1] (rename-node val t)
                        old-name (n/var-name var)
                        new-name (u/fresh-name old-name)
                        t2 (assoc t1 old-name new-name)
                        new-var (n/make-variable new-name (n/attrs var) (n/node-meta var) (n/parent var))]
                    [(conj bnds [new-var new-val]) t2]))
                [[] table]
                old-bindings)
        [new-body table2] (rename-node (n/loop-body node) table1)]
    [(n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :catch [node table]
  (let [old-sym (n/catch-sym node)
        old-name (n/var-name old-sym)
        new-name (u/fresh-name old-name)
        table1 (assoc table old-name new-name)
        [new-class table2] (rename-node (n/catch-class node) table1)
        [new-body-exprs table3]
        (reduce (fn [[exprs t] expr]
                  (let [[e t2] (rename-node expr t)]
                    [(conj exprs e) t2]))
                [[] table2]
                (n/catch-body node))]
    [(n/make-catch new-class
                   (n/make-variable new-name (n/attrs old-sym) (n/node-meta old-sym) (n/parent old-sym))
                   new-body-exprs
                   (n/attrs node) (n/node-meta node) (n/parent node))
     table3]))

;; ── 容器节点：递归传递表 ────────────────
(defmethod rename-node :call [node table]
  (let [[new-fn table1] (rename-node (n/call-fn node) table)
        [new-args table2]
        (reduce (fn [[args t] arg]
                  (let [[a t2] (rename-node arg t)]
                    [(conj args a) t2]))
                [[] table1]
                (n/call-args node))]
    [(n/make-call new-fn new-args (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :if [node table]
  (let [[test table1] (rename-node (n/if-test node) table)
        [then table2] (rename-node (n/if-then node) table1)
        [else table3] (if-let [e (n/if-else node)]
                        (rename-node e table2)
                        [nil table2])]
    [(n/make-if test then else (n/attrs node) (n/node-meta node) (n/parent node))
     table3]))

(defmethod rename-node :block [node table]
  (let [[new-exprs table2]
        (reduce (fn [[exprs t] expr]
                  (let [[e t2] (rename-node expr t)]
                    [(conj exprs e) t2]))
                [[] table]
                (n/block-exprs node))]
    [(n/make-block new-exprs (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :assign [node table]
  (let [[new-var table1] (rename-node (n/assign-var node) table)
        [new-val table2] (rename-node (n/assign-val node) table1)]
    [(n/make-assign new-var new-val (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :recur [node table]
  (let [[new-args table2]
        (reduce (fn [[args t] arg]
                  (let [[a t2] (rename-node arg t)]
                    [(conj args a) t2]))
                [[] table]
                (n/recur-args node))]
    [(n/make-recur new-args (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :try [node table]
  (let [[new-body table1] (rename-node (n/try-body node) table)
        [new-catches table2]
        (reduce (fn [[cs t] c]
                  (let [[nc t2] (rename-node c t)]
                    [(conj cs nc) t2]))
                [[] table1]
                (n/try-catches node))
        [new-finally table3] (if-let [f (n/try-finally node)]
                               (rename-node f table2)
                               [nil table2])]
    [(n/make-try new-body new-catches new-finally (n/attrs node) (n/node-meta node) (n/parent node))
     table3]))

(defmethod rename-node :throw [node table]
  (let [[new-expr table2] (rename-node (n/throw-expr node) table)]
    [(n/make-throw new-expr (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :while [node table]
  (let [[test table1] (rename-node (n/while-test node) table)
        [body table2] (rename-node (n/while-body node) table1)]
    [(n/make-while test body (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :vector [node table]
  (let [[new-items table2]
        (reduce (fn [[items t] item]
                  (let [[i t2] (rename-node item t)]
                    [(conj items i) t2]))
                [[] table]
                (n/vector-items node))]
    [(n/make-vector new-items (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :map [node table]
  (let [[new-kvs table2]
        (reduce (fn [[kvs t] kv]
                  (let [[kv2 t2] (rename-node kv t)]
                    [(conj kvs kv2) t2]))
                [[] table]
                (n/map-kvs node))]
    [(n/make-map new-kvs (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :define [node table]
  (let [[new-val table2] (rename-node (n/define-val node) table)]
    [(n/make-define (n/define-name node) new-val (n/define-doc node)
                    (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :convert [node table]
  (let [[new-expr table2] (rename-node (n/convert-expr node) table)]
    [(n/make-convert new-expr (n/convert-src-ty node) (n/convert-dst-ty node) (n/convert-cost node)
                     (n/attrs node) (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :member-access [node table]
  (let [[new-target table1] (rename-node (n/access-target node) table)
        [new-args table2]
        (reduce (fn [[args t] arg]
                  (let [[a t2] (rename-node arg t)]
                    [(conj args a) t2]))
                [[] table1]
                (n/access-args node))]
    [(n/make-member-access new-target (n/access-member node) new-args
                           (n/node-meta node) (n/parent node))
     table2]))

;; ── 数组特殊节点 ────────────────────────
(defmethod rename-node :new-array [node table]
  (let [[new-size table2] (rename-node (n/new-array-size node) table)]
    [(n/make-new-array new-size (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :aget [node table]
  (let [[new-target table1] (rename-node (n/aget-target node) table)
        [new-idx table2] (rename-node (n/aget-idx node) table1)]
    [(n/make-aget new-target new-idx (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :aset [node table]
  (let [[new-target table1] (rename-node (n/aset-target node) table)
        [new-idx table2] (rename-node (n/aset-idx node) table1)
        [new-val table3] (rename-node (n/aset-val node) table2)]
    [(n/make-aset new-target new-idx new-val (n/node-meta node) (n/parent node))
     table3]))

(defmethod rename-node :alength [node table]
  (let [[new-target table2] (rename-node (n/alength-target node) table)]
    [(n/make-alength new-target (n/node-meta node) (n/parent node))
     table2]))

(defmethod rename-node :ns [node table] [node table])
(defmethod rename-node :record [node table] [node table])
(defmethod rename-node :protocol [node table] [node table])

(defmethod rename-node :default [node table] [node table])

;; ── 入口 ──────────────────────────────────
(defn rename
  "对单个 IR2 节点或根列表进行 alpha 重命名。返回新节点/列表。"
  ([node]
   (let [[new-node _] (rename-node node {})]
     new-node))
  ([node table]
   (rename-node node table)))

(defn rename-roots
  "对 IR2 根节点列表进行 alpha 重命名，返回新列表。"
  [roots]
  (first (reduce (fn [[new-roots table] root]
                   (let [[new-root new-table] (rename-node root table)]
                     [(conj new-roots new-root) new-table]))
                 [[] {}]
                 roots)))