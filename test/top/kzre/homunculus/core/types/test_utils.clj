(ns top.kzre.homunculus.core.types.test-utils
  "公共测试工具，提供模拟前端、后端、类型断言以及高阶消除配置等。"
  (:require
    [clojure.walk :as w]
    [top.kzre.homunculus.core.ir2.model :as m]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]
    [top.kzre.homunculus.core.types.model :as t]
    [top.kzre.homunculus.core.types.protocol :as tp])
  (:import
    [top.kzre.homunculus.core.types.model
     TCon TContainer TFun TVar]))

;; ── 通用 Mock 前端 ──
(defrecord MockFrontend []
  tp/IFrontendInfo
  (frontend-types [_] [:int64 :float64 :bool :string :keyword :nil :vector :map])
  (literal->type [_ val]
    (cond
      (integer? val) (t/->TCon :int64)
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

;; ── HLSL 测试用前端 ──
(defrecord MockHLSLFrontend []
  tp/IFrontendInfo
  (frontend-types [_] [:int :float :bool :float2 :float3 :float4 :float4x4 :texture2D :sampler :vector :map])
  (literal->type [_ val]
    (cond
      (integer? val) (t/->TCon :int)
      (float? val)   (t/->TCon :float)
      (true? val)    (t/->TCon :bool)
      (false? val)   (t/->TCon :bool)
      (keyword? val) (t/->TCon :keyword)
      (nil? val)     (t/->TCon :float)
      :else (t/->TVar (gensym "lit"))))
  (meta->type [_ node]
    (when-let [tag (or (get-in node [:meta :tag])
                       (get-in node [:attrs :tag]))]
      (if (keyword? tag)
        (t/->TCon tag)
        (t/->TCon (keyword (name tag))))))
  (infer-collection-type [_ form] nil)
  (collection-type-ctor [_ kind element-type shape] nil))

;; ── Mock 后端 ──
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

;; ── 高阶消除配置 Mock ──
(defrecord MockHoElimConfig []
  hop/IHoElimConfig
  (known-ho-functions [_] {'reduce :reduce, 'map :map})
  (supports-dynamic-collections? [_] false)
  (backend-length-fn [_] 'count)
  (backend-nth-fn [_] 'nth)
  (backend-less-than-fn [_] '<))

;; ── 类型断言辅助 ──
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

(defn convert? [node]
  (= (:kind node) :convert))

(defn convert-cost [node] (get-in node [:attrs :cost]))

;; ── 快速构造辅助 ──
(defn with-meta-var [name meta]
  (m/->VariableNode name nil meta nil))

(defn macroexpand-deep [form]
  (w/postwalk (fn [f] (if (seq? f) (macroexpand f) f)) form))