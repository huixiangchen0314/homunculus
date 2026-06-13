(ns top.kzre.homunculus.core.types.test-utils
  "公共测试工具，提供模拟前端和常用类型断言。"
  (:require
   [top.kzre.homunculus.core.ir2.protocol :as ir2p]
   [top.kzre.homunculus.core.types.model :as t ]
   [top.kzre.homunculus.core.types.protocol :as tp])
  (:import
    [top.kzre.homunculus.core.types.model TCon TContainer TFun TVar]))

(defrecord MockFrontend []
  tp/IFrontendInfo
  (frontend-types [_] [:int64 :float64 :bool :string :keyword :nil])
  (literal->type [_ val]
    (cond
      (integer? val) (t/->TCon :int64)   ;; 所有整数均视为 int64
      (float? val)   (t/->TCon :float64)
      (string? val)  (t/->TCon :string)
      (true? val)    (t/->TCon :bool)
      (false? val)   (t/->TCon :bool)
      (keyword? val) (t/->TCon :keyword)
      (nil? val)     (t/->TCon :nil)
      :else          (throw (ex-info "Unsupported literal" {:val val}))))
  (meta->type [_ node]
    (when-let [tag (or (get-in node [:meta :tag])
                       (get-in node [:attrs :tag]))]
      (if (keyword? tag)
        (t/->TCon tag)
        (t/->TCon (keyword (name tag))))))
  (infer-collection-type [_ form] nil)
  (collection-type-ctor [_ kind element-type shape] nil))

(defn get-type [node]
  (cond
    (satisfies? ir2p/INode node) (let [a (ir2p/attrs node)]
                                   (when a (:type a)))
    (map? node) (-> node :attrs :type)
    :else nil))

(defn tcon? [ty name]
  (cond
    (instance? TCon ty) (= (:name ty) name)
    (instance? TContainer ty) (= (:kind ty) name)
    :else false))

(defn tfun? [ty] (instance? TFun ty))
(defn tvar? [ty] (instance? TVar ty))

;; 在文件末尾添加

(defrecord MockBackend []
  tp/IBackendInfo
  (prims [_] [])
  (builtin-type? [_ ty-name] true)
  (strictness [_] {:type false})
  (type-conversion [_ src dst]
    (when (and (instance? TCon src) (instance? TCon dst)
               (= (:name src) :int64)
               (= (:name dst) :float32))
      1))
  (resolve-container [_ container-ty] nil)
  (backend-container-type [_ kind element-ty shape] nil))

(defn convert? [node]
  (= (:kind node) :convert))

(defn convert-cost [node] (get-in node [:attrs :cost]))