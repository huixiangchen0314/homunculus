(ns top.kzre.homunculus.core.ir2.pass.constraint.env
  (:require
    [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
    [top.kzre.homunculus.internal.protocol :as proto]))

(defrecord Env [ctx])

(defn make-env
  [ctx]
  (->Env ctx))

(defn conversion-cost [env src-ty dst-ty]
  (let [be (proto/backend (:ctx env))]
    (tp/type-conversion be src-ty dst-ty)))