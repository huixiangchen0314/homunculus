(ns top.kzre.homunculus.internal.model
  "编译配置和上下文的默认实现。"
  (:require
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.internal.module-unit :as mu]
    [top.kzre.homunculus.internal.protocol :as p]
    [top.kzre.homunculus.internal.utils :as u]))

(defrecord CompileConfig [options source-paths lib-paths output-dir target module-naming-style]
  p/ICompileConfig
  (options [_] options)
  (source-paths [_] source-paths)
  (lib-paths [_] lib-paths)
  (output-dir [_] output-dir)
  (target [_] (or target :hlsl))
  (module-naming-style [_] (or module-naming-style :default)))

(defrecord CompileTarget [frontend backend compiler emitter])

(defn make-compile-target
  [frontend backend compiler emitter]
  (->CompileTarget frontend backend compiler emitter))

(defn make-compile-config
  [options]
  (let [target        (keyword (get options :target "hlsl"))
        lib-paths     (or (not-empty (:lib options)) ["."])
        source-paths  (or (:include options) [])
        output-dir    (:output options)
        style         (or (:style options) :default)]
    (->CompileConfig options source-paths lib-paths output-dir target style)))


(defrecord CompileContextState [compiling
                                modules
                                symbol-table])


(defn- ensure-compiled [ctx ns-sym]
  (when-not (p/module-unit ctx ns-sym)
    (let [state-atom (:state-atom ctx)]
      (when (contains? (get state-atom :compiling) ns-sym)
        (throw (ex-info "Circular dependency" {:module ns-sym})))
      (swap! state-atom update :compiling conj ns-sym)
      (try
        ;; 从 lib-paths 加载源文件，解析表单，然后调用 compile-module
        (let [lib-paths (p/lib-paths (p/config ctx))
              src       (u/resolve-module lib-paths ns-sym)
              _         (when-not src
                          (throw (ex-info "Module not found" {:module ns-sym :paths lib-paths})))
              forms     (u/parse-forms src)]
          (p/compile (p/compiler ctx) forms ctx))
        (finally
          (swap! state-atom update :compiling disj ns-sym))))))

(defn resolve-module-symbol-table
  "解析当前命名空间可见符号表，假设依赖图已无环。"
  [ctx ns-sym]
  (let [unit     (p/module-unit ctx ns-sym)
        table    (mu/module-public-symbols unit)
        requires (mu/module-requires unit)]  ;; 仅保留合格的 ns 符号
    (apply merge
           (map #(resolve-module-symbol-table ctx %) requires)
           table)))

(defrecord DefaultCompileContext [config
                                  ^CompileTarget target
                                  state-atom]
  p/ICompileContext
  (config [_] config)
  (frontend [_] (:frontend target))
  (backend [_] (:backend target))
  (compiler [_] (:compiler target))
  (emitter [_] (:emitter target))
  (register-deps [this dep-syms]
    (doseq [dep dep-syms]
      (ensure-compiled this dep))
    this)  ;; 返回自身

  (register-sym [this sym-entry]
    (swap! state-atom assoc-in [:symbol-table (:sym sym-entry)] sym-entry)
    this)
  (symbols [this ns-sym]
    (let [user-table (resolve-module-symbol-table this ns-sym)
          frontend (p/frontend this)
          builtin-table (tp/builtin-symbols frontend)]
      (merge builtin-table user-table)))
  (symbol-table [this]
    (let [user-table (:symbol-table @state-atom)
          frontend (p/frontend this)
          builtin-table (tp/builtin-symbols frontend)]
      (merge builtin-table user-table)))
  (module-unit [_ ns-sym]
    (get-in @state-atom [:modules ns-sym]))
  (set-module-unit! [_ unit]
    (if-let [ns-sym (mu/module-ns unit)]
      (swap! state-atom assoc-in [:modules ns-sym] unit)
      (throw (ex-info "ns is require for module unit" {:unit unit}))))
  (all-module-units [_] (vals (get-in @state-atom [:modules]))))

(defn make-compile-context
  [config target]
  (let [init-state (->CompileContextState #{} {} {})
        state-atom (atom init-state)]
    (->DefaultCompileContext config target state-atom)))
