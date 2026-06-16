(ns top.kzre.homunculus.core.types.ho-elim.methods.map
  "消除 (map f coll)，支持固定长度向量与变长集合。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]
            [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]))

(defn- build-static-map [f-node items]
  (let [new-items (mapv (fn [item] (n/make-call f-node [item] {} nil nil)) items)]
    (n/make-vector new-items nil nil nil)))

(defn- build-dynamic-map [f-node coll-node config]
  (let [len-sym  'len
        idx-sym  'idx
        out-sym  'out
        len-call (n/make-call (n/make-variable (hop/backend-length-fn config) nil nil nil)
                              [coll-node] {} nil nil)
        elem-ty  (some-> coll-node ty/get-type ty/container-element-type)
        out-ctor (hop/backend-variable-collection-ctor config
                                                       (n/make-variable len-sym nil nil nil)
                                                       elem-ty)
        init-bindings [[(n/make-variable idx-sym nil nil nil) (n/make-literal 0 nil nil nil)]
                       [(n/make-variable out-sym nil nil nil) out-ctor]]
        less-fn  (hop/backend-less-than-fn config)
        condition (n/make-call (n/make-variable less-fn nil nil nil)
                               [(n/make-variable idx-sym nil nil nil) (n/make-variable len-sym nil nil nil)]
                               {} nil nil)
        nth-fn   (hop/backend-nth-fn config)
        item-node (n/make-call (n/make-variable nth-fn nil nil nil)
                               [coll-node (n/make-variable idx-sym nil nil nil)]
                               {} nil nil)
        mapped-val (n/make-call f-node [item-node] {} nil nil)
        set-out   (hop/backend-variable-collection-set config
                                                       (n/make-variable out-sym nil nil nil)
                                                       (n/make-variable idx-sym nil nil nil)
                                                       mapped-val)
        add-fn    (hop/backend-add-fn config)
        next-idx  (n/make-call (n/make-variable add-fn nil nil nil)
                               [(n/make-variable idx-sym nil nil nil) (n/make-literal 1 nil nil nil)]
                               {} nil nil)
        loop-body (n/make-block [set-out
                                 (n/make-assign (n/make-variable idx-sym nil nil nil) next-idx nil nil nil)
                                 (n/make-recur [(n/make-variable idx-sym nil nil nil)
                                                (n/make-variable out-sym nil nil nil)]
                                               nil nil nil)]
                                nil nil nil)
        loop-node (n/make-loop init-bindings loop-body nil nil nil)]
    (n/make-block [(n/make-let [[(n/make-variable len-sym nil nil nil) len-call]] loop-node nil nil nil)
                   (n/make-variable out-sym nil nil nil)]
                  nil nil nil)))

(defn expand-map [f-node coll-node config]
  (let [coll-ty    (ty/get-type coll-node)
        shape      (when coll-ty (ty/container-shape coll-ty))
        shape-kind (when shape (ty/shape-kind shape))]
    (if (and shape (= :fixed shape-kind))
      (build-static-map f-node (n/vec-items coll-node))
      (if (hop/supports-variable-collections? config)
        (build-dynamic-map f-node coll-node config)
        (throw (ex-info "map requires a fixed-length vector" {:coll coll-node}))))))