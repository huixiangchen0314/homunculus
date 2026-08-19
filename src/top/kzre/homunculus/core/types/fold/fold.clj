(ns top.kzre.homunculus.core.types.fold.fold
  "常量折叠 Pass：递归遍历 IR2 树，利用后端实现的 IFolder 协议进行常量折叠。"
  (:require [top.kzre.homunculus.core.ir2.ast :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.fold.protocol :as p]
            [top.kzre.homunculus.core.types.type :as ty]))

;; env 积累数组长度进行折叠
(defrecord Env [ctx folder])

(defn make-env
  [ctx folder]
  (->Env ctx folder))

(defmulti fold-node (fn [node _env] (n/kind node)))

(defn walk
  [node env]
  (ir2/reduce-children node fold-node env))

;; ── 调用节点：先递归处理子节点，再尝试折叠 ──
(defmethod fold-node :call [node env]
  (let [[node' env'] (walk node env)]
    [(or (p/fold-node (:folder env) node' (:ctx env))
         node')
     env']))

(defmethod fold-node :alength
 [node env]
 (let [[node' env'] (walk node env)
       target (n/alength-target node')
       t (ty/get-type target)]
   (if (ty/vec-type? t)
     (let [sz (ty/vec-size t)
           len (when (ty/type-value? sz) (ty/value-val sz))]
       (if (integer? len)
         [(n/make-literal len (ir2/attrs node) (ir2/node-meta node))
          env']
         [node' env']))
     [node' env'])))

(defmethod fold-node :default [node env]
  (walk node env))


;; ── 入口 ──
(defn fold
  [ir2-roots folder ctx]
  (let [init-env (make-env ctx folder)]
    (mapv #(first (fold-node % init-env)) ir2-roots)))