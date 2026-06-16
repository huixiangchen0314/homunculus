(ns top.kzre.homunculus.core.types.ho-elim.core
  "高阶函数消除 Pass。根据前端配置将高阶调用展开为一阶形式。
   所有节点操作均通过 ir2.node 工具函数。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.ho-elim.methods.map :as map-expand]
            [top.kzre.homunculus.core.types.ho-elim.methods.reduce :as reduce-expand]
            [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]))

;; ── 多方法分派 ──────────────────────────
(defmulti eliminate
          (fn [node config] (n/kind node)))

;; ── 叶子节点 ────────────────────────────
(defmethod eliminate :literal [node _] node)
(defmethod eliminate :variable [node _] node)

;; ── 调用（高阶消除核心）───────────────
(defmethod eliminate :call [node config]
  (let [fn-node   (n/call-fn node)
        fn-name   (some-> fn-node n/var-name)
        ho-map    (hop/known-ho-functions config)
        strategy  (get ho-map (symbol fn-name))]
    (case strategy
      :reduce
      (let [args (n/call-args node)]
        (if (= 3 (count args))
          (let [f-node    (first args)
                init-node (second args)
                coll-node (nth args 2)]
            (if (= (n/kind coll-node) :vector)
              (reduce-expand/expand-reduce f-node init-node coll-node config)
              ;; 非向量，递归处理后重建
              (n/make-call (eliminate fn-node config)
                           (mapv #(eliminate % config) args)
                           (n/attrs node) (n/node-meta node) (n/parent node))))
          (throw (ex-info "reduce requires exactly 3 arguments" {:node node}))))

      :map
      (let [args (n/call-args node)]
        (if (= 2 (count args))
          (let [f-node    (first args)
                coll-node (second args)]
            (if (= (n/kind coll-node) :vector)
              (map-expand/expand-map f-node coll-node config)
              (n/make-call (eliminate fn-node config)
                           (mapv #(eliminate % config) args)
                           (n/attrs node) (n/node-meta node) (n/parent node))))
          (throw (ex-info "map requires exactly 2 arguments" {:node node}))))

      ;; 普通调用
      (n/make-call (eliminate fn-node config)
                   (mapv #(eliminate % config) (n/call-args node))
                   (n/attrs node) (n/node-meta node) (n/parent node)))))

;; ── 控制流容器 ─────────────────────────
(defmethod eliminate :if [node config]
  (n/make-if (eliminate (n/if-test node) config)
             (eliminate (n/if-then node) config)
             (when-let [e (n/if-else node)] (eliminate e config))
             (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate :block [node config]
  (n/make-block (mapv #(eliminate % config) (n/block-exprs node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate :let [node config]
  (let [new-bindings (mapv (fn [[v e]] [(eliminate v config) (eliminate e config)])
                           (n/let-bindings node))
        new-body     (eliminate (n/let-body node) config)]
    (n/make-let new-bindings new-body
                (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod eliminate :loop [node config]
  (let [new-bindings (mapv (fn [[v e]] [(eliminate v config) (eliminate e config)])
                           (n/loop-bindings node))
        new-body     (eliminate (n/loop-body node) config)]
    (n/make-loop new-bindings new-body
                 (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod eliminate :while [node config]
  (n/make-while (eliminate (n/while-test node) config)
                (eliminate (n/while-body node) config)
                (n/attrs node) (n/node-meta node) (n/parent node)))

;; ── 函数定义与顶层定义 ────────────────
(defmethod eliminate :lambda [node config]
  (n/make-lambda (mapv #(eliminate % config) (n/lambda-params node))
                 (eliminate (n/lambda-body node) config)
                 (n/lambda-captures node) (n/lambda-fn-name node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate :define [node config]
  (if-let [val (n/define-val node)]
    (n/make-define (n/define-name node)
                   (eliminate val config)
                   (n/define-doc node)
                   (n/attrs node) (n/node-meta node) (n/parent node))
    node))

;; ── 赋值 ───────────────────────────────
(defmethod eliminate :assign [node config]
  (n/make-assign (eliminate (n/assign-var node) config)
                 (eliminate (n/assign-val node) config)
                 (n/attrs node) (n/node-meta node) (n/parent node)))

;; ── 异常处理 ───────────────────────────
(defmethod eliminate :try [node config]
  (n/make-try (eliminate (n/try-body node) config)
              (mapv #(eliminate % config) (n/try-catches node))
              (when-let [f (n/try-finally node)] (eliminate f config))
              (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate :catch [node config]
  (n/make-catch (n/catch-class node) (n/catch-sym node)
                (mapv #(eliminate % config) (n/catch-body node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate :throw [node config]
  (n/make-throw (eliminate (n/throw-expr node) config)
                (n/attrs node) (n/node-meta node) (n/parent node)))

;; ── 集合 ───────────────────────────────
(defmethod eliminate :vector [node config]
  (n/make-vector (mapv #(eliminate % config) (n/vector-items node))
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate :map [node config]
  (n/make-map (mapv #(eliminate % config) (n/map-kvs node))
              (n/attrs node) (n/node-meta node) (n/parent node)))

;; ── 其他无子节点或无需处理 ────────────
(defmethod eliminate :recur [node config]
  (n/make-recur (mapv #(eliminate % config) (n/recur-args node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate :convert [node config]
  (n/make-convert (eliminate (n/convert-expr node) config)
                  (n/convert-src-ty node) (n/convert-dst-ty node) (n/convert-cost node)
                  (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate :member-access [node config]
  (n/make-member-access (eliminate (n/access-target node) config)
                        (n/access-member node)
                        (mapv #(eliminate % config) (n/access-args node))
                        (n/node-meta node) (n/parent node)))


(defmethod eliminate :record [node config]
  (n/make-record (n/record-name node)
                 (mapv (fn [field]
                         (if-let [init (n/field-init field)]
                           (n/field-with-init field (eliminate init config))
                           field))
                       (n/record-fields node))
                 (n/record-protocols node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate :ns [node _] node)


(defmethod eliminate :protocol [node _] node)

(defmethod eliminate :default [node _]
  (throw (ex-info (str "Unknown node kind in ho-elim: " (n/kind node)) {:node node})))

;; ── 对外入口 ────────────────────────────
(defn process
  "对 IR2 根节点列表执行高阶消除。"
  [ir2-roots config]
  (mapv #(eliminate % config) ir2-roots))