(ns top.kzre.homunculus.core.types.constraint.constraint
  "约束记录的构造函数与访问器。"
  (:require [top.kzre.homunculus.core.types.constraint.model :as cm]))

;; ── CEqual ──────────────────────────────
(defn make-cequal [tvar type]
  (cm/->CEqual tvar type))

(defn cequal-tvar [ceq] (:tvar ceq))
(defn cequal-type [ceq] (:type ceq))

;; ── COverload ───────────────────────────
(defn make-coverload [fn-ty-list arg-tys ret-tvar node]
  (cm/->COverload fn-ty-list arg-tys ret-tvar node))

(defn coverload-fn-ty-list [cov] (:fn-ty-list cov))
(defn coverload-arg-tys    [cov] (:arg-tys cov))
(defn coverload-ret-tvar   [cov] (:ret-tvar cov))
(defn coverload-node       [cov] (:node cov))

;; ── CConvert ────────────────────────────
(defn make-cconvert [node src-ty dst-ty cost]
  (cm/->CConvert node src-ty dst-ty cost))

(defn cconvert-node   [ccv] (:node ccv))
(defn cconvert-src-ty [ccv] (:src-ty ccv))
(defn cconvert-dst-ty [ccv] (:dst-ty ccv))
(defn cconvert-cost   [ccv] (:cost ccv))