(ns top.kzre.homunculus.backend.glsl.semantic.config
  (:require
    [top.kzre.homunculus.backend.glsl.config :as cfg]
    [top.kzre.homunculus.internal.protocol :as ip]))

(defprotocol IConfig
  (gl-version [_])
  (out-struct-strategy [_])
  (fragment-out-name [_])
  (semantic-location-strategy [_])
  (vertex-semantic-location [_ semantic])
  (varying-location-enabled? [_])
  (varying-semantic-location [_ semantic])
  (fragment-semantic-location [_ semantic])
  (texture-sampler-binding-strategy [_])
  (sampler-naming-strategy [_])
  (map-texture-sampler-binding [_ tex-reg sampler-reg])
  (interface-block-naming [_ struct-name])
  (interface-block-instance-naming [_ struct-name stage direction]))

(defrecord Config [compile-ctx config ]
  IConfig
  (gl-version [_]
    (cfg/gl-version config))

  (out-struct-strategy [_]
    (cfg/out-struct-strategy config))

  (fragment-out-name [_]
    (cfg/fragment-out-name config))

  (semantic-location-strategy [_]
    (cfg/semantic-location-strategy config))

  (vertex-semantic-location [_ semantic]
    (cfg/vertex-semantic-location config semantic))
  (varying-location-enabled? [_]
    (cfg/varying-location-enabled? config))
  (varying-semantic-location [_ semantic]
    (cfg/varying-semantic-location config semantic))

  (fragment-semantic-location [_ semantic]
    (cfg/fragment-semantic-location config semantic))

  (texture-sampler-binding-strategy [_]
    (cfg/texture-sampler-binding-strategy config))
  (sampler-naming-strategy [_]
    (cfg/sampler-naming-strategy config))
  (map-texture-sampler-binding [_ tex-reg sampler-reg]
    (let [v (cfg/texture-sampler-binding config)]
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
    (let [v (cfg/interface-block-naming config)]
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
    (let [v (cfg/interface-block-instance-naming config)]
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
  (->Config compile-ctx (ip/config compile-ctx) ))