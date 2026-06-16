(ns top.kzre.homunculus.core.types.recur-elim.methods.catch
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :catch [node]
  (n/make-catch (rec/eliminate (n/catch-class node))
                (rec/eliminate (n/catch-sym node))
                (mapv rec/eliminate (n/catch-body node))
                (n/attrs node) (n/node-meta node) (n/parent node)))