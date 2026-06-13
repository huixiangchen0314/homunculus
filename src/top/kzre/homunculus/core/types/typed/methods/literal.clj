(ns top.kzre.homunculus.core.types.typed.methods.literal
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :literal [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [frontend (:frontend context)
          ty (or (when frontend (tp/meta->type frontend node))
                 (when frontend (tp/literal->type frontend (:val node)))
                 (t/->TVar (gensym "lit")))
          new-node (type/set-type! node ty)]
      [ty new-node {}])))