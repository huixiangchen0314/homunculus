(ns top.kzre.homunculus.core.types.test-utils
  "公共测试工具，提供模拟前端和常用类型断言。"
  (:require [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(defrecord MockFrontend []
  tp/IFrontendInfo
  (frontend-types [_] [:int64 :float64 :bool :string :keyword :nil])
  (literal->type [_ val]
    (cond
      (instance? java.lang.Long val)    (t/->TCon :int64)
      (instance? java.lang.Double val)  (t/->TCon :float64)
      (instance? java.lang.Boolean val) (t/->TCon :bool)
      (instance? java.lang.String val)  (t/->TCon :string)
      (keyword? val)                    (t/->TCon :keyword)
      (nil? val)                        (t/->TCon :nil)
      :else (throw (ex-info "Unknown literal" {:val val}))))
  (meta->type [_ node]
    (when-let [tag (or (get-in node [:meta :tag])
                       (get-in node [:attrs :tag]))]
      (if (keyword? tag)
        (t/->TCon tag)
        (t/->TCon (keyword (name tag))))))
  (infer-collection-type [_ form] nil)
  (collection-type-ctor [_ kind element-type shape] nil))

(defn get-type [node] (-> node ir2p/attrs :type))

(defn tcon? [ty name]
  (and (instance? TCon ty) (= name (:name ty))))

(defn tfun? [ty] (instance? TFun ty))
(defn tvar? [ty] (instance? TVar ty))