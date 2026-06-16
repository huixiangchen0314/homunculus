(ns top.kzre.homunculus.core.types.lambda-elim.methods.define
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :define [node roots config defs]
  (n/make-define (n/define-name node)
                 (elim/eliminate (n/define-val node) roots config defs)
                 (n/define-doc node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))