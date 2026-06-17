(ns top.kzre.homunculus.backend.hlsl.methods.resource
  "HLSL 资源声明节点发射（虚拟 :define-resource）。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]
            [clojure.string :as str]))

(defmethod core/emit-node :define-resource [node]
  (let [attrs    (n/attrs node)
        res-kind (:shader/resource-kind attrs)
        res-name (name (n/define-name node))
        val-node (n/define-val node)
        reg      (when val-node (n/lit-val val-node))]
    (case res-kind
      :texture2D (tmpl/texture2d-decl res-name reg)
      :sampler   (tmpl/sampler-decl res-name reg)
      :cbuffer   (let [members-str
                       (when val-node
                         (let [args (n/call-args val-node)]
                           (str/join "\n"
                                     (map (fn [m] (tmpl/struct-member
                                                    (core/hlsl-type-str (ty/get-type m))
                                                    (name (n/var-name m))
                                                    nil))
                                          args))))]
                   (tmpl/cbuffer-decl res-name reg members-str))
      (throw (ex-info "Unknown resource type" {:node node})))))