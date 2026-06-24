(ns top.kzre.homunculus.internal.model
  "编译配置和上下文的默认实现。"
  (:require
    [top.kzre.homunculus.internal.protocol :as p]
    [top.kzre.homunculus.internal.utils :as u]))

(defn get-module-unit [context ns-sym]
  (get-in @(:state context) [:modules ns-sym]))

(defn set-module-unit! [context ns-sym unit]
  (swap! (:state context) assoc-in [:modules ns-sym] unit))


(defrecord CompileConfig [options source-paths lib-paths output-dir target module-naming-style]
  p/ICompileConfig
  (options [_] options)
  (source-paths [_] source-paths)
  (lib-paths [_] lib-paths)
  (output-dir [_] output-dir)
  (target [_] (or target :hlsl))
  (module-naming-style [_] (or module-naming-style :default)))


(defn- ensure-compiled [ctx ns-sym]
  (when-not (get-module-unit ctx ns-sym)
    (let [state (:state ctx)]
      (when (contains? (get @state :compiling) ns-sym)
        (throw (ex-info "Circular dependency" {:module ns-sym})))
      (swap! state update :compiling conj ns-sym)
      (try
        ;; 从 lib-paths 加载源文件，解析表单，然后调用 compile-module
        (let [lib-paths (p/lib-paths (p/config ctx))
              src       (u/resolve-module lib-paths ns-sym)
              _         (when-not src
                          (throw (ex-info "Module not found" {:module ns-sym :paths lib-paths})))
              forms     (u/parse-forms src)]
          (p/compile-module (:compiler ctx) forms ctx))
        (finally
          (swap! state update :compiling disj ns-sym))))))


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

