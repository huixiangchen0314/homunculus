;; ═══════════════════════════════════════════════════════
;; ir2/forms/variable.clj
;; ═══════════════════════════════════════════════════════
(ns top.kzre.homunculus.core.ir2.forms.variable
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :var [node env]
  (let [kid (first (ir1p/children node))]
    (ir2/lower-ast kid env)))   ;; 直接返回变量引用