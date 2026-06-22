(ns top.kzre.homunculus.core.types.lambda-elim.methods.member-access
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :member-access [node config env]
  (let [[new-target target-defs] (elim/eliminate (n/access-target node) config env)
        [new-args args-defs]
        (reduce (fn [[args defs] arg]
                  (let [[new-arg arg-defs] (elim/eliminate arg config env)]
                    [(conj args new-arg) (into defs arg-defs)]))
                [[] []]
                (n/access-args node))]
    [(n/make-member-access new-target (n/access-member node) new-args
                           (n/node-meta node) (n/parent node))
     (into target-defs args-defs)]))