(ns top.kzre.homunculus.core.types.lambda-elim.methods.catch
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :catch [node roots config defs]
  (n/make-catch (elim/eliminate (n/catch-class node) roots config defs)
                (elim/eliminate (n/catch-sym node) roots config defs)
                (mapv #(elim/eliminate % roots config defs) (n/catch-body node))
                (n/attrs node) (n/node-meta node) (n/parent node)))