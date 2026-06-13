(ns top.kzre.homunculus.core.types.ho-elim.core
  "高阶函数消除 Pass。根据前端配置将高阶调用展开为一阶形式。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.node :as node]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]
            [top.kzre.homunculus.core.types.ho-elim.methods.reduce :as reduce-expand]
            [top.kzre.homunculus.core.types.ho-elim.methods.map :as map-expand]))

(defmulti eliminate
          (fn [node config] (ir2p/kind node)))

(defmethod eliminate :literal [node _] node)
(defmethod eliminate :variable [node _] node)

(defmethod eliminate :call [node config]
  (let [fn-name (some-> (node/call-fn node) node/var-name)
        ho-map  (hop/known-ho-functions config)
        strategy (get ho-map (symbol fn-name))]
    (case strategy
      :reduce
      (let [args (node/call-args node)]
        (if (= 3 (count args))
          (let [f-node (first args)
                init-node (second args)
                coll-node (nth args 2)]
            (if (= (node/kind coll-node) :vector)
              (reduce-expand/expand-reduce f-node init-node coll-node config)
              (let [new-fn (eliminate (node/call-fn node) config)
                    new-args (mapv #(eliminate % config) args)]
                (node/->call new-fn new-args (node/attrs node) (node/node-meta node) (node/parent node)))))
          (throw (ex-info "reduce requires exactly 3 arguments" {:node node}))))

      :map
      (let [args (node/call-args node)]
        (if (= 2 (count args))
          (let [f-node (first args)
                coll-node (second args)]
            (if (= (node/kind coll-node) :vector)
              (map-expand/expand-map f-node coll-node config)
              (let [new-fn (eliminate (node/call-fn node) config)
                    new-args (mapv #(eliminate % config) args)]
                (node/->call new-fn new-args (node/attrs node) (node/node-meta node) (node/parent node)))))
          (throw (ex-info "map requires exactly 2 arguments" {:node node}))))

      ;; 非目标函数
      (let [new-fn (eliminate (node/call-fn node) config)
            new-args (mapv #(eliminate % config) (node/call-args node))]
        (node/->call new-fn new-args (node/attrs node) (node/node-meta node) (node/parent node))))))

(defmethod eliminate :if [node config]
  (node/->if (eliminate (node/if-test node) config)
             (eliminate (node/if-then node) config)
             (when-let [e (node/if-else node)] (eliminate e config))
             (node/attrs node) (node/node-meta node) (node/parent node)))

(defmethod eliminate :while [node config]
  (node/->while (eliminate (node/while-test node) config)
                (eliminate (node/while-body node) config)
                (node/attrs node) (node/node-meta node) (node/parent node)))

(defmethod eliminate :block [node config]
  (node/->block (mapv #(eliminate % config) (node/block-exprs node))
                (node/attrs node) (node/node-meta node) (node/parent node)))

(defmethod eliminate :let [node config]
  (let [new-bindings (mapv (fn [[v e]] [(eliminate v config) (eliminate e config)]) (node/let-bindings node))
        new-body (eliminate (node/let-body node) config)]
    (node/->let new-bindings new-body (node/attrs node) (node/node-meta node) (node/parent node))))

(defmethod eliminate :lambda [node config]
  (node/->lambda (mapv #(eliminate % config) (node/lambda-params node))
                 (eliminate (node/lambda-body node) config)
                 (node/lambda-captures node) (node/lambda-fn-name node)
                 (node/attrs node) (node/node-meta node) (node/parent node)))

(defmethod eliminate :define [node config]
  (if-let [val (node/define-val node)]
    (node/->define (node/define-name node)
                   (eliminate val config)
                   (node/define-doc node)
                   (node/attrs node) (node/node-meta node) (node/parent node))
    node))

(defmethod eliminate :assign [node config]
  (node/->assign (eliminate (node/assign-var node) config)
                 (eliminate (node/assign-val node) config)
                 (node/attrs node) (node/node-meta node) (node/parent node)))

(defmethod eliminate :try [node config]
  (node/->try (mapv #(eliminate % config) (node/try-body node))
              (mapv #(eliminate % config) (node/try-catches node))
              (when-let [f (node/try-finally node)] (mapv #(eliminate % config) f))
              (node/attrs node) (node/node-meta node) (node/parent node)))

(defmethod eliminate :catch [node config]
  (node/->catch (node/catch-class node) (node/catch-sym node)
                (mapv #(eliminate % config) (node/catch-body node))
                (node/attrs node) (node/node-meta node) (node/parent node)))

(defmethod eliminate :throw [node config]
  (node/->throw (eliminate (node/throw-expr node) config)
                (node/attrs node) (node/node-meta node) (node/parent node)))

(defmethod eliminate :vector [node config]
  (node/->vector (mapv #(eliminate % config) (node/vec-items node))
                 (node/attrs node) (node/node-meta node) (node/parent node)))

(defmethod eliminate :map [node config]
  (node/->map (mapv #(eliminate % config) (node/map-kvs node))
              (node/attrs node) (node/node-meta node) (node/parent node)))

(defmethod eliminate :default [node _]
  (throw (ex-info (str "Unknown node kind in ho-elim: " (ir2p/kind node)) {:node node})))

(defn process
  "对 IR2 根节点列表执行高阶消除。"
  [ir2-roots config]
  (mapv #(eliminate % config) ir2-roots))