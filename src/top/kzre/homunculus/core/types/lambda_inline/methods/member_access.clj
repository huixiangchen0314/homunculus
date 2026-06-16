(ns top.kzre.homunculus.core.types.lambda-inline.methods.member-access
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :member-access [node config]
  (n/make-member-access (inline/eliminate-inline (n/access-target node) config)
                        (n/access-member node)   ;; accessor 不变
                        (mapv #(inline/eliminate-inline % config) (n/access-args node))
                        (n/node-meta node) (n/parent node)))