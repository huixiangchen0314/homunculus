(ns top.kzre.homunculus.core.types.test-utils
  "公共测试工具，提供模拟前端、后端、类型断言以及高阶消除配置等。"
  (:require
    [clojure.walk :as w]
    [top.kzre.homunculus.core.ir2.model :as m]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]
    [top.kzre.homunculus.core.types.model :as t]
    [top.kzre.homunculus.internal.protocol :as ip]
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
  (collection-type-ctor [_ kind element-type shape] nil)

  ;; ── 新增：内置函数类型环境 ──
  (builtin-functions [_]
    {'+      (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
     '-      (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
     '*      (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
     '/      (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
     'inc    (t/->TFun (t/->TCon :int64) (t/->TCon :int64))
     'dec    (t/->TFun (t/->TCon :int64) (t/->TCon :int64))
     'zero?  (t/->TFun (t/->TCon :int64) (t/->TCon :bool))
     '=      (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :bool)))
     '<      (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :bool)))
     '>      (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :bool)))
     'not    (t/->TFun (t/->TCon :bool) (t/->TCon :bool))
     'and    (t/->TFun (t/->TCon :bool) (t/->TFun (t/->TCon :bool) (t/->TCon :bool)))
     'or     (t/->TFun (t/->TCon :bool) (t/->TFun (t/->TCon :bool) (t/->TCon :bool)))
     'str    (t/->TFun (t/->TCon :int64) (t/->TCon :string))}))

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
  (collection-type-ctor [_ kind element-type shape] nil)
  (builtin-functions [_] {}))    ;; HLSL 测试无需内置函数

;; ── Mock 后端 ──
(defrecord MockBackend []
  tp/IBackendInfo
  (prims [_] [])
  (builtin-type? [_ ty-name] true)
  (strictness [_] {:type false})
  (type-conversion [_ src dst]
    ;; 允许 int64 -> float32 转换，代价为 1
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

;; ── 便捷环境构建 ──
(defn builtin-env
  "返回一个包含 MockFrontend 内置函数类型的本地环境 map。"
  ([]
   (builtin-env (->MockFrontend)))
  ([frontend]
   (into {} (map (fn [[sym ty]] [sym ty]) (tp/builtin-functions frontend)))))


;; ── Mock 编译配置 ──
(defrecord MockCompileConfig []
  ip/ICompileConfig
  (source-paths [_] ["src"])
  (lib-paths [_] ["lib"])
  (output-dir [_] "out"))

(defrecord MockCompileContext [state]
ip/ICompileContext
(config [_] (:config @state))

(register-deps [this _dep-syms]
               ;; 测试中通常不触发真实编译，直接返回自身
               this)

(lookup-type [_ full-name]
             (get-in @state [:types full-name]))

(get-export-syms [_ ns-sym]
                 (get-in @state [:exports ns-sym])))


;; ── 工厂函数：创建 Mock 编译上下文 ──
(defn ->mock-compile-ctx
  "构建测试用编译上下文。
   可选参数：
     :config  - ICompileConfig 实例，默认 MockCompileConfig
     :exports - {ns-sym {sym export-entry}}  导出表
     :types   - {fully-qualified-sym type}   类型查找表"
  [& {:keys [config exports types]
      :or   {config (->MockCompileConfig)
             exports {}
             types   {}}}]
  (->MockCompileContext (atom {:config  config
                               :exports exports
                               :types   types})))