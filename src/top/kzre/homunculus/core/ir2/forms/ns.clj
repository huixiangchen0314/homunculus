(ns top.kzre.homunculus.core.ir2.forms.ns
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]
            ))

(defmethod ir2/lower-ast :ns [node env]
 (m/->NsNode (:name node)
             (:docstring node)
             (:attr-map node)
             (:references node)
             nil
             (:meta node)
             nil))
