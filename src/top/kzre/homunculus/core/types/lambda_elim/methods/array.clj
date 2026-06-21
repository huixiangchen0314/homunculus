(ns top.kzre.homunculus.core.types.lambda-elim.methods.array
  "为 lambda-elim Pass 处理数组特殊节点：递归消除子节点中的闭包。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :new-array [node roots config defs]
  (n/make-new-array (elim/eliminate (n/new-array-size node) roots config defs)
                    (n/node-meta node)
                    (n/parent node)))

(defmethod elim/eliminate :aget [node roots config defs]
  (n/make-aget (elim/eliminate (n/aget-target node) roots config defs)
               (elim/eliminate (n/aget-idx node) roots config defs)
               (n/node-meta node)
               (n/parent node)))

(defmethod elim/eliminate :aset [node roots config defs]
  (n/make-aset (elim/eliminate (n/aset-target node) roots config defs)
               (elim/eliminate (n/aset-idx node) roots config defs)
               (elim/eliminate (n/aset-val node) roots config defs)
               (n/node-meta node)
               (n/parent node)))

(defmethod elim/eliminate :alength [node roots config defs]
  (n/make-alength (elim/eliminate (n/alength-target node) roots config defs)
                  (n/node-meta node)
                  (n/parent node)))
