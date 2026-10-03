(ns top.kzre.homunculus.backend.hlsl.lang
  (:require
   [top.kzre.homunculus.backend.format :refer [T]]
   [top.kzre.homunculus.core.error :as err]
   [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defn type->str [ir-type]
  (cond
    (ty/vec-type? ir-type)
    (let [elem-type (type->str (ty/vec-element-type ir-type))
          size (ty/value-val (ty/vec-size ir-type))]
      (T "${elem-type}[${size}]"))
    (ty/type-sym ir-type) (name (ty/type-sym ir-type))
    ;:else (throw (ex-info (str "Unknown type: " ir-type) {}))
    :else (pr-str ir-type)
    ))

(defn literal->str [val]
  (cond
    (nil? val)   nil
    (integer? val) (str val)
    (float? val)   (str val)
    (true? val)    "true"
    (false? val)   "false"
    :else (throw
            (err/compile-error
              :unsupported-literal-type
              (str "Unsupported literal type for value: " (pr-str val))))))

(defn sematic->str [kw stage]
  (case kw
    :position (if (= stage :vertex) "POSITION" "SV_POSITION")
    :normal       "NORMAL"
    :tangent      "TANGENT"
    :texcoord0    "TEXCOORD0"
    :texcoord1    "TEXCOORD1"
    :texcoord2    "TEXCOORD2"
    :texcoord3    "TEXCOORD3"
    :texcoord4    "TEXCOORD4"
    :texcoord5    "TEXCOORD5"
    :texcoord6    "TEXCOORD6"
    :texcoord7    "TEXCOORD7"
    :color0       "COLOR0"
    :color1       "COLOR1"
    :target0      "SV_TARGET"
    :target1      "SV_TARGET1"
    :depth        "SV_DEPTH"
    :instance-id  "SV_INSTANCEID"
    :vertex-id    "SV_VERTEXID"
    :primitive-id "SV_PRIMITIVEID"
    :user0        "USER0"
    :user1        "USER1"
    :user2        "USER2"
    :user3        "USER3"
    (throw (err/compile-error
             :unknown-hlsl-sematic
             (str "Unknow HLSL sematic: " kw)
             ))))