;; ═══════════════════════════════════════════════════════
;; ir2/forms/quote.clj
;; ═══════════════════════════════════════════════════════
(ns top.kzre.homunculus.core.ir2.forms.quote
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :quote [node env]
  (let [kid (first (ir1p/children node))
        lowered (first (ir2/lower-ast kid env))]
    [lowered]))  ;; quote 直接返回内部表达式，不保留 quote 节点