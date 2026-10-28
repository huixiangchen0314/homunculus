(ns top.kzre.homunculus.frontend.shader.pass.loopfor.env
  (:require
    [top.kzre.homunculus.internal.protocol :as ip]))

(defprotocol IEnv
  (all-loopfor? [_] "是否将所有while循环转成 for 循环. true 的话进行退化形式转换，否则只转换自然循环"))

(defrecord Env [ compile-ctx]
  IEnv
  (all-loopfor? [_]
    (or (-> compile-ctx
            (ip/config)
            (ip/get-option :shader-all-loopfor?))
        true)))

(defn make-env [compile-ctx]
  (->Env compile-ctx ))