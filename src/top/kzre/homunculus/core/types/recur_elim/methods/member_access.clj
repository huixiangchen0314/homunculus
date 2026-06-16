;; recur_elim/methods/member_access.clj
(ns top.kzre.homunculus.core.types.recur-elim.methods.member-access
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :member-access [node]
  (n/make-member-access (rec/eliminate (n/access-target node))
                        (n/access-member node)          ;; accessor 不变
                        (mapv rec/eliminate (n/access-args node))
                        (n/node-meta node) (n/parent node)))