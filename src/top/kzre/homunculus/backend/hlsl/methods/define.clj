(ns top.kzre.homunculus.backend.hlsl.methods.define
  "HLSL :define 节点发射。"
  (:require
    [top.kzre.homunculus.backend.hlsl.core :as core]
    [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [clojure.string :as str]))

(defmethod core/emit-node :define [node]
  (let [val (n/define-val node)]
    (if (= (n/kind val) :lambda)
      (let [lam        val
            params     (n/lambda-params lam)
            ret-type   (core/hlsl-type-str (ty/fun-ret (ty/get-type lam)))
            param-strs (mapv (fn [p] (str (core/hlsl-type-str (ty/get-type p)) " " (name (n/var-name p)))) params)
            body       (core/emit-node (n/lambda-body lam))
            func-name  (name (n/define-name node))]
        (str (tmpl/func-signature ret-type func-name (str/join ", " param-strs)) "\n" (tmpl/func-body body)))
      (tmpl/var-decl-init (core/hlsl-type-str (ty/get-type val)) (name (n/define-name node)) (core/emit-node val)))))