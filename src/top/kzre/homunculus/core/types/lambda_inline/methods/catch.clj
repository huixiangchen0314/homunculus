(ns top.kzre.homunculus.core.types.lambda-inline.methods.catch
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :catch [node config]
  (n/make-catch (inline/eliminate-inline (n/catch-class node) config)
                (inline/eliminate-inline (n/catch-sym node) config)
                (mapv #(inline/eliminate-inline % config) (n/catch-body node))
                (n/attrs node) (n/node-meta node) (n/parent node)))