(ns top.kzre.homunculus.internal.model
  "编译配置和上下文的默认实现。"
  (:require
    [clojure.spec.alpha :as s]
    [top.kzre.homunculus.internal.protocol :as p]
    [top.kzre.homunculus.internal.spec :as spec]
    [top.kzre.homunculus.internal.utils :as u]))

(defrecord CompileConfig [source-paths lib-paths output-dir]
  p/ICompileConfig
  (source-paths [_] source-paths)
  (lib-paths [_] lib-paths)
  (output-dir [_] output-dir)
  (module-naming-style [_] :default))


(defn- ensure-compiled
  "确保 ns-sym 已被编译并缓存，否则递归编译。"
  [ctx ns-sym]
  (let [s (:state ctx)]
    (when-not (get-in @s [:modules ns-sym])
      ;; 循环依赖检测
      (when (get @s :compiling)
        (when (contains? (get @s :compiling) ns-sym)
          (throw (ex-info "Circular dependency" {:module ns-sym}))))
      ;; 1. 标记为“编译中”
      (swap! s update :compiling conj ns-sym)
      (try
        ;; 2. 加载源文件
        (let [lib-paths (p/lib-paths (:config ctx))
              src       (u/resolve-module lib-paths ns-sym)]
          (when-not src
            (throw (ex-info "Module not found" {:module ns-sym :paths lib-paths})))
          (let [forms (u/parse-forms src)
                result (p/emit (:compiler ctx) forms ctx)]
            ;; 简单验证结果
            (when (s/valid? ::spec/emit-result result)
              ;; 缓存结果
              (swap! s assoc-in [:modules ns-sym] result))
            result))
        (finally
          ;; 移除“编译中”标记
          (swap! s update :compiling disj ns-sym))))))

;; state 应该是个 atom
(defrecord DefaultCompileContext [config compiler state]
  p/ICompileContext
  (config [_] config)

  (register-deps [this dep-syms]
    (doseq [dep dep-syms]
      (ensure-compiled this dep))
    this)  ;; 返回自身

  (register-sym [this sym-entry]
    (swap! state assoc-in [:symbol-table (:sym sym-entry)] sym-entry)
    this)

  (symbol-table [_] (:symbol-table @state)))