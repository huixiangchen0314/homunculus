(ns top.kzre.homunculus.core.types.lambda-elim.methods.array
  "为 lambda-elim Pass 处理数组特殊节点：递归消除子节点中的闭包。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :new-array [node config env]
  (let [[new-size size-defs] (elim/eliminate (n/new-array-size node) config env)]
    [(n/make-new-array new-size (n/node-meta node) (n/parent node))
     size-defs]))

(defmethod elim/eliminate :aget [node config env]
  (let [[new-target target-defs] (elim/eliminate (n/aget-target node) config env)
        [new-idx idx-defs]       (elim/eliminate (n/aget-idx node) config env)]
    [(n/make-aget new-target new-idx (n/node-meta node) (n/parent node))
     (into target-defs idx-defs)]))

(defmethod elim/eliminate :aset [node config env]
  (let [[new-target target-defs] (elim/eliminate (n/aset-target node) config env)
        [new-idx idx-defs]       (elim/eliminate (n/aset-idx node) config env)
        [new-val val-defs]       (elim/eliminate (n/aset-val node) config env)]
    [(n/make-aset new-target new-idx new-val (n/node-meta node) (n/parent node))
     (into target-defs (into idx-defs val-defs))]))

(defmethod elim/eliminate :alength [node config env]
  (let [[new-target target-defs] (elim/eliminate (n/alength-target node) config env)]
    [(n/make-alength new-target (n/node-meta node) (n/parent node))
     target-defs]))