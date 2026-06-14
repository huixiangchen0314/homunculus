(ns top.kzre.homunculus.cli.spec
  "编译配置的 clojure.spec 规格定义。"
  (:require [clojure.spec.alpha :as s]))

(s/def ::source-paths (s/coll-of string? :kind vector? :min-count 1))
(s/def ::lib-paths    (s/coll-of string? :kind vector?))
(s/def ::output-dir   string?)
(s/def ::backend      #{:hlsl :glsl :unity :lua :csharp})

(s/def ::compile-config
  (s/keys :req-un [::source-paths]
          :opt-un [::lib-paths ::output-dir ::backend]))