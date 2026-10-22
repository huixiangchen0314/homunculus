(ns top.kzre.homunculus.internal.config-impl
  (:require
    [top.kzre.homunculus.internal.protocol :as proto]
    [top.kzre.homunculus.internal.utils :as util]))

(defrecord Config
  [path
   options])

(def ^:dynamic *default-config*
  "内置默认配置，作为优先级最低的 Config 参与 resolve-option。
   仅包含“编译配置”类字段；:help / :version / :config 是 CLI 控制流，
   不属于编译配置，由 core.clj 直接读 :options，不在此定义。"
  (->Config
    "top.kzre.homunculus.internal.config-impl/defaults"
    {:output        "out"
     :target        "hlsl"
     :lib           ["."]
     :style         :default
     :split-modules false
     :called-driven-interface? true
     :macro-ns      []}))

(defn- ensure-path-vector
  "把 keyword 或序列统一成 vector。
   注意原实现用 seq?，对 vector 输入返回 false，会把 [:a :b] 包成 [[:a :b]]，
   这里改用 sequential?。"
  [path]
  (if (sequential? path)
    path
    [path]))

(def ^:private not-found ::not-found)

(defn- get-in-or
  "安全 get-in：区分“key 不存在”与“key 存在但值为 nil”。
   返回 not-found 哨兵表示路径上某一层 key 不存在。"
  [m path]
  (reduce (fn [acc k]
            (if (and (map? acc) (contains? acc k))
              (get acc k)
              (reduced not-found)))
          m
          path))

(defn- provided?
  "该值是否视为“显式提供”：存在且非 nil。
   配合注册选项时使用 :default nil，才能把“未提供”和“提供了 nil”区分开。"
  [v]
  (and (not= v not-found) (some? v)))

(defn- resolve-option
  "按优先级解析选项路径 path 的值。

   path         - keyword 序列，如 [:target] 或 [:backend :glsl :version]
   cfgs         - Config 序列，按命令行出现顺序排列（后面的优先级更高，
                  符合 -c a.edn -c b.edn 时 b 覆盖 a 的直觉）
   cli-options - 命令行解析结果 map（tools.cli 输出的 :options）

   优先级：命令行显式值 > 后面的配置文件 > 前面的配置文件 > 无。
   命令行里值为 nil 的 key 视为未提供，从而让配置文件的值生效。

   返回 {:config cfg-or-nil, :value v}：
     - 来自命令行时 :config 为 nil；
     - 来自某个配置文件时 :config 为该 Config record（可用于诊断来源）；
     - 都未命中时 {:config nil :value nil}。"
  [path cfgs cli-options]
  (let [cli-val (get-in-or cli-options path)]
    (if (provided? cli-val)
      {:config nil :value cli-val}
      (loop [cs (reverse (seq cfgs))]
        (if-let [cfg (first cs)]
          (let [v (get-in-or (:options cfg) path)]
            (if (provided? v)
              {:config cfg :value v}
              (recur (rest cs))))
          {:config nil :value nil})))))

(defrecord CompileConfig [options configs target]
  proto/ICompileConfig
  (options [_] options)
  (get-option [_ path]
    (let [{:keys [value]}
          (resolve-option (ensure-path-vector path)
                          (conj configs *default-config*)
                          options)]
      value))
  (lib-paths [this] (proto/get-option this :lib))
  (output-path [this] (proto/get-option this :output))
  (target [_] target)
  (module-naming-style [this] (proto/get-option this :style)))

(defn make-compile-config
  [options & configs]
  (let [target (util/ensure-keyword
                 (or (:target options)
                     (throw (ex-info "target must be no nil." {}))))]
    ;; 后指定的配置优先级高
    (->CompileConfig options (reverse configs) target)))