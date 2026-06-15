(ns top.kzre.homunculus.core.types.constraint.gen.methods.try
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :try [node context]
  (let [tv (gen/fresh-tvar)]
    [tv (ty/set-type! node tv) nil]))

(defmethod gen/cg-node-raw :catch [node context]
  (let [tv (gen/fresh-tvar)]
    [tv (ty/set-type! node tv) nil]))

(defmethod gen/cg-node-raw :throw [node context]
  (let [tv (gen/fresh-tvar)]
    [tv (ty/set-type! node tv) nil]))