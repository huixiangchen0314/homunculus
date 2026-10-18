(ns top.kzre.homunculus.backend.glsl.sematic.config
  (:require
   [top.kzre.homunculus.backend.glsl.options :as opt]
   [top.kzre.homunculus.backend.glsl.sematic.location :as location]
   [top.kzre.homunculus.internal.protocol :as ip]))

(defprotocol IConfig
  (gl-version [_])
  (out-struct-strategy [_])
  (fragment-out-name [_])
  (vertex-semantic-location [_ semantic])
  (fragment-semantic-location [_ semantic])
  (texture-sampler-binding-strategy [_])
  (sampler-naming-strategy [_])
  (map-texture-sampler-binding [_ tex-reg sampler-reg])
  (interface-block-naming [_ struct-name])
  (interface-block-instance-naming [_ struct-name stage direction]))

(defrecord Config [compile-ctx options]
  IConfig
  (gl-version [_]
    (opt/gl-version options))

  (out-struct-strategy [_]
    (opt/out-struct-strategy options))

  (fragment-out-name [_]
    (opt/fragment-out-name options))

  (vertex-semantic-location [_ semantic]
    (or (opt/vertex-semantic-location options semantic)
        (location/default-location semantic)))

  (fragment-semantic-location [_ semantic]
    (opt/fragment-semantic-location options semantic))

  (texture-sampler-binding-strategy [_]
    (opt/texture-sampler-binding-strategy options))
  (sampler-naming-strategy [_]
    (opt/sampler-naming-strategy options))
  (map-texture-sampler-binding [_ tex-reg sampler-reg]
    (let [v (opt/texture-sampler-binding options)]
      (cond
        (fn? v)
        (v tex-reg sampler-reg)

        (map? v)
        (let [unit (get v [tex-reg sampler-reg])]
          (if (nil? unit)
            (throw (ex-info "no binding mapping for combination"
                            {:tex-reg tex-reg :sampler-reg sampler-reg
                             :map-keys (keys v)}))
            unit))

        (nil? v)
        (throw (ex-info "mapping strategy requires :gl-texture-sampler-binding"
                        {:tex-reg tex-reg :sampler-reg sampler-reg}))

        :else
        (throw (ex-info "unsupported :gl-texture-sampler-binding type"
                        {:type (type v) :value v})))))

  (interface-block-naming [_ struct-name]
    (let [v (opt/interface-block-naming options)]
      (cond
        (fn? v)
        (v struct-name)

        (map? v)
        (or (get v struct-name)
            (throw (ex-info "no interface-block name for struct"
                            {:struct-name struct-name
                             :map-keys    (keys v)})))

        (nil? v)
        (name struct-name)

        :else
        (throw (ex-info "unsupported :gl-interface-block-naming type"
                        {:type (type v) :value v})))))

  (interface-block-instance-naming [_ struct-name stage direction]
    (let [v (opt/interface-block-instance-naming options)]
      (cond
        (fn? v)
        (v struct-name stage direction)

        (nil? v)
        (str "_"
             (name struct-name) "_"
             (name direction) "_"
             (Integer/toHexString
               (hash [struct-name stage direction])))

        :else
        (throw (ex-info "unsupported :gl-interface-block-instance-naming type"
                        {:type (type v) :value v}))))))

(defn make-config [compile-ctx]
  (let [options (ip/options (ip/config compile-ctx))]
    (->Config compile-ctx options)))