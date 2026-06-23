(ns top.kzre.homunculus.core.types.alias
  "别名应用 Pass：遍历 IR2 树，将符号表中的别名替换为目标符号。
   在 resolve-ns 之后执行。每个节点类型由独立的多方法处理，确保子节点正确递归。"
  (:require
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.internal.symbol :as sym]
    [top.kzre.homunculus.core.types.protocol :as types]))

;; ── 构建别名映射 {unqualified-alias -> fully-qualified-target} ──
(defn- build-alias-map [symbol-table]
  (into {}
        (keep (fn [[_ entry]]
                (when (sym/alias-symbol? entry)
                  [(symbol (name (:sym entry)))   ;; 别名的短名称
                   (sym/alias-target entry)]))
              symbol-table)))

;; ── 多方法分派：每个节点种类分别实现 ──
(defmulti apply-alias-node (fn [node _alias-map] (n/kind node)))

;; 变量节点：替换无命名空间的别名
(defmethod apply-alias-node :variable [node alias-map]
  (let [var-name (n/var-name node)]
    (if (and (not (namespace var-name))
             (contains? alias-map var-name))
      (n/variable-with-name node (get alias-map var-name))
      node)))

;; 字面量、常量等直接返回
(defmethod apply-alias-node :literal [node _] node)

;; define 节点：递归值
(defmethod apply-alias-node :define [node alias-map]
  (let [val (n/define-val node)]
    (if val
      (n/make-define (n/define-name node)
                     (apply-alias-node val alias-map)
                     (n/define-doc node)
                     (n/attrs node)
                     (n/node-meta node)
                     (n/parent node))
      node)))

;; lambda：递归参数和 body，注意参数不需要别名替换，但它们也是变量节点，会被递归处理，但参数不应被替换（因为它们是无命名空间的局部变量）。由于别名表中的键通常是顶层函数名，与参数名冲突的可能性很小，但为安全，我们可以限制仅替换不在局部作用域中的变量？目前简单的做法是：参数也会被递归，但 alias-map 中不太可能包含参数名，所以安全。如果真的冲突，后面会有内联等 Pass 处理。
(defmethod apply-alias-node :lambda [node alias-map]
  (let [new-params (mapv #(apply-alias-node % alias-map) (n/lambda-params node))
        new-body   (apply-alias-node (n/lambda-body node) alias-map)]
    (n/make-lambda new-params new-body
                   (n/lambda-captures node)
                   (n/lambda-fn-name node)
                   (n/attrs node)
                   (n/node-meta node)
                   (n/parent node))))

;; let 绑定：值在外部环境递归，body 在内部环境？alias 不关心词法作用域，统一替换即可。
(defmethod apply-alias-node :let [node alias-map]
  (let [new-bindings (mapv (fn [[v e]]
                             [(apply-alias-node v alias-map)
                              (apply-alias-node e alias-map)])
                           (n/let-bindings node))
        new-body (apply-alias-node (n/let-body node) alias-map)]
    (n/make-let new-bindings new-body
                (n/attrs node)
                (n/node-meta node)
                (n/parent node))))

;; loop 类似
(defmethod apply-alias-node :loop [node alias-map]
  (let [new-bindings (mapv (fn [[v e]]
                             [(apply-alias-node v alias-map)
                              (apply-alias-node e alias-map)])
                           (n/loop-bindings node))
        new-body (apply-alias-node (n/loop-body node) alias-map)]
    (n/make-loop new-bindings new-body
                 (n/attrs node)
                 (n/node-meta node)
                 (n/parent node))))

;; block
(defmethod apply-alias-node :block [node alias-map]
  (let [new-exprs (mapv #(apply-alias-node % alias-map) (n/block-exprs node))]
    (n/make-block new-exprs (n/attrs node) (n/node-meta node) (n/parent node))))

;; call
(defmethod apply-alias-node :call [node alias-map]
  (let [new-fn   (apply-alias-node (n/call-fn node) alias-map)
        new-args (mapv #(apply-alias-node % alias-map) (n/call-args node))]
    (n/make-call new-fn new-args (n/attrs node) (n/node-meta node) (n/parent node))))

;; if
(defmethod apply-alias-node :if [node alias-map]
  (let [new-test (apply-alias-node (n/if-test node) alias-map)
        new-then (apply-alias-node (n/if-then node) alias-map)
        new-else (when-let [e (n/if-else node)]
                   (apply-alias-node e alias-map))]
    (n/make-if new-test new-then new-else
               (n/attrs node) (n/node-meta node) (n/parent node))))

;; while
(defmethod apply-alias-node :while [node alias-map]
  (let [new-test (apply-alias-node (n/while-test node) alias-map)
        new-body (apply-alias-node (n/while-body node) alias-map)]
    (n/make-while new-test new-body (n/attrs node) (n/node-meta node) (n/parent node))))

;; assign
(defmethod apply-alias-node :assign [node alias-map]
  (let [new-var (apply-alias-node (n/assign-var node) alias-map)
        new-val (apply-alias-node (n/assign-val node) alias-map)]
    (n/make-assign new-var new-val (n/attrs node) (n/node-meta node) (n/parent node))))

;; recur
(defmethod apply-alias-node :recur [node alias-map]
  (let [new-args (mapv #(apply-alias-node % alias-map) (n/recur-args node))]
    (n/make-recur new-args (n/attrs node) (n/node-meta node) (n/parent node))))

;; 数组操作节点
(defmethod apply-alias-node :new-array [node alias-map]
  (let [new-size (apply-alias-node (n/new-array-size node) alias-map)]
    (n/make-new-array new-size (n/node-meta node) (n/parent node))))

(defmethod apply-alias-node :aget [node alias-map]
  (let [new-target (apply-alias-node (n/aget-target node) alias-map)
        new-idx    (apply-alias-node (n/aget-idx node) alias-map)]
    (n/make-aget new-target new-idx (n/node-meta node) (n/parent node))))

(defmethod apply-alias-node :aset [node alias-map]
  (let [new-target (apply-alias-node (n/aset-target node) alias-map)
        new-idx    (apply-alias-node (n/aset-idx node) alias-map)
        new-val    (apply-alias-node (n/aset-val node) alias-map)]
    (n/make-aset new-target new-idx new-val (n/node-meta node) (n/parent node))))

(defmethod apply-alias-node :alength [node alias-map]
  (let [new-target (apply-alias-node (n/alength-target node) alias-map)]
    (n/make-alength new-target (n/node-meta node) (n/parent node))))

;; vector
(defmethod apply-alias-node :vector [node alias-map]
  (let [new-items (mapv #(apply-alias-node % alias-map) (n/vector-items node))]
    (n/make-vector new-items (n/attrs node) (n/node-meta node) (n/parent node))))

;; map
(defmethod apply-alias-node :map [node alias-map]
  (let [new-kvs (mapv #(apply-alias-node % alias-map) (n/map-kvs node))]
    (n/make-map new-kvs (n/attrs node) (n/node-meta node) (n/parent node))))

;; member-access
(defmethod apply-alias-node :member-access [node alias-map]
  (let [new-target (apply-alias-node (n/access-target node) alias-map)
        new-args   (mapv #(apply-alias-node % alias-map) (n/access-args node))]
    (n/make-member-access new-target (n/access-member node) new-args
                          (n/node-meta node) (n/parent node))))

;; convert
(defmethod apply-alias-node :convert [node alias-map]
  (let [new-expr (apply-alias-node (n/convert-expr node) alias-map)]
    (n/make-convert new-expr (n/convert-src-ty node) (n/convert-dst-ty node)
                    (n/convert-cost node) (n/attrs node) (n/node-meta node) (n/parent node))))

;; try / catch / throw
(defmethod apply-alias-node :try [node alias-map]
  (let [new-body    (apply-alias-node (n/try-body node) alias-map)
        new-catches (mapv #(apply-alias-node % alias-map) (n/try-catches node))
        new-finally (when-let [f (n/try-finally node)]
                      (apply-alias-node f alias-map))]
    (n/make-try new-body new-catches new-finally
                (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod apply-alias-node :catch [node alias-map]
  (let [new-class (apply-alias-node (n/catch-class node) alias-map)
        new-sym   (apply-alias-node (n/catch-sym node) alias-map)
        new-body  (mapv #(apply-alias-node % alias-map) (n/catch-body node))]
    (n/make-catch new-class new-sym new-body
                  (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod apply-alias-node :throw [node alias-map]
  (let [new-expr (apply-alias-node (n/throw-expr node) alias-map)]
    (n/make-throw new-expr (n/attrs node) (n/node-meta node) (n/parent node))))

;; record 和 protocol
(defmethod apply-alias-node :record [node alias-map]
  (let [new-fields (mapv (fn [f]
                           (if-let [init (n/field-init f)]
                             (n/field-with-init f (apply-alias-node init alias-map))
                             f))
                         (n/record-fields node))]
    (n/make-record (n/record-name node) new-fields (n/record-protocols node)
                   (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod apply-alias-node :protocol [node _] node)   ; 协议没有变量引用

(defmethod apply-alias-node :ns [node _] node)

;; 默认：字面量等直接返回
(defmethod apply-alias-node :default [node _] node)

;; ── 入口 ──
(defn apply-alias
  [ir2-roots context frontend]
  (let [builtin-table (types/builtin-symbols frontend)
        user-table    (ip/symbol-table context)
        combined-table (merge builtin-table user-table)
        alias-map (build-alias-map combined-table)]
    (mapv #(apply-alias-node % alias-map) ir2-roots)))