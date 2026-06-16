(ns top.kzre.homunculus.core.types.ho-elim.methods.reduce
  "消除 (reduce f init coll)，支持固定长度向量与变长集合。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]
            [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]))

(defn- build-static-reduce [f-node init-node items]
  (if (empty? items)
    init-node
    (reduce (fn [acc item] (n/make-call f-node [acc item] {} nil nil))
            init-node items)))

(defn- build-dynamic-reduce [f-node init-node coll-node config]
  (let [len-sym  'len
        idx-sym  'idx
        acc-sym  'acc
        len-call (n/make-call (n/make-variable (hop/backend-length-fn config) nil nil nil)
                              [coll-node] {} nil nil)
        init-bindings [[(n/make-variable idx-sym nil nil nil) (n/make-literal 0 nil nil nil)]
                       [(n/make-variable acc-sym nil nil nil) init-node]]
        less-fn  (hop/backend-less-than-fn config)
        condition (n/make-call (n/make-variable less-fn nil nil nil)
                               [(n/make-variable idx-sym nil nil nil) (n/make-variable len-sym nil nil nil)]
                               {} nil nil)
        nth-fn   (hop/backend-nth-fn config)
        item-node (n/make-call (n/make-variable nth-fn nil nil nil)
                               [coll-node (n/make-variable idx-sym nil nil nil)]
                               {} nil nil)
        new-acc  (n/make-call f-node [(n/make-variable acc-sym nil nil nil) item-node] {} nil nil)
        add-fn   (hop/backend-add-fn config)
        next-idx (n/make-call (n/make-variable add-fn nil nil nil)
                              [(n/make-variable idx-sym nil nil nil) (n/make-literal 1 nil nil nil)]
                              {} nil nil)
        loop-body (n/make-block [(n/make-assign (n/make-variable acc-sym nil nil nil) new-acc nil nil nil)
                                 (n/make-assign (n/make-variable idx-sym nil nil nil) next-idx nil nil nil)
                                 (n/make-recur [(n/make-variable idx-sym nil nil nil)
                                                (n/make-variable acc-sym nil nil nil)]
                                               nil nil nil)]
                                nil nil nil)
        loop-node (n/make-loop init-bindings loop-body nil nil nil)]
    (n/make-block [(n/make-let [[(n/make-variable len-sym nil nil nil) len-call]] loop-node nil nil nil)
                   (n/make-variable acc-sym nil nil nil)]
                  nil nil nil)))

(defn expand-reduce [f-node init-node coll-node config]
  (let [coll-ty    (ty/get-type coll-node)
        shape      (when coll-ty (ty/container-shape coll-ty))
        shape-kind (when shape (ty/shape-kind shape))]
    (if (and shape (= :fixed shape-kind))
      (build-static-reduce f-node init-node (n/vec-items coll-node))
      (if (hop/supports-variable-collections? config)
        (build-dynamic-reduce f-node init-node coll-node config)
        (throw (ex-info "reduce requires a fixed-length vector" {:coll coll-node}))))))