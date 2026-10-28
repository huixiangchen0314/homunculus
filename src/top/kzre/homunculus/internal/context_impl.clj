(ns top.kzre.homunculus.internal.context-impl
  "编译配置和上下文的默认实现。"
  (:require
   [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
   [top.kzre.homunculus.internal.module-unit :as mu]
   [top.kzre.homunculus.internal.protocol :as proto]
   [top.kzre.homunculus.internal.resolve-lib :as resolve-lib]
   [top.kzre.homunculus.internal.utils :as util])
  (:import
   (top.kzre.homunculus.internal.compile_target CompileTarget)))



(defrecord CompileContextState
  [compiling
   modules
   symbol-table
   loaded-macro-ns])

(defn- ensure-compiled [ctx ns-sym]
  (when-not (proto/module-unit ctx ns-sym)
    (let [state-atom (:state-atom ctx)]
      (when (contains? (get state-atom :compiling) ns-sym)
        (throw (ex-info "Circular dependency" {:module ns-sym})))
      (swap! state-atom update :compiling conj ns-sym)
      (try
        ;; 从 lib-paths 加载源文件，解析表单，然后调用 compile-module
        (let [lib-paths (proto/lib-paths (proto/config ctx))
              src       (resolve-lib/read-ns-source lib-paths ns-sym)
              _         (when-not src
                          (throw (ex-info "Module not found" {:module ns-sym :paths lib-paths})))
              forms     (util/parse-forms src)]
          (proto/compile (proto/compiler ctx) forms ctx))
        (finally
          (swap! state-atom update :compiling disj ns-sym))))))


(defrecord DefaultCompileContext [config
                                  ^CompileTarget target
                                  state-atom]
  proto/ICompileContext
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
  (symbol-table [this]
    (let [user-table (:symbol-table @state-atom)
          frontend (proto/frontend this)
          builtin-table (tp/builtin-symbols frontend)]
      (merge builtin-table user-table)))
  (module-unit [_ ns-sym]
    (get-in @state-atom [:modules ns-sym]))
  (set-module-unit! [_ unit]
    (if-let [ns-sym (mu/module-ns unit)]
      (swap! state-atom assoc-in [:modules ns-sym] unit)
      (throw (ex-info "ns is require for module unit" {:unit unit}))))
  (all-module-units [_] (vals (get-in @state-atom [:modules])))
  (builtin-instruction? [_ sym]
    (and (symbol? sym)
         (nil? (namespace sym))
         (.startsWith ^String (name sym) "%%")))
  (macro-namespaces [this]
    (let [builtin (some-> (proto/frontend this)
                          tp/macro-namespaces
                          (or #{}))
          extra   (set (proto/get-option config :macro-ns))]
      (into builtin extra)))
  (load-macro-ns! [this ns-sym]
    (let [s @state-atom]
      (if (contains? (:loaded-macro-ns s #{}) ns-sym)
        this
        (let [lib-paths (proto/lib-paths config)
              ;; 1. 已在 JVM classpath：frontend 内置宏 / 已 require 过的 ns
              in-jvm?   (or (find-ns ns-sym)
                            (try (require ns-sym) (find-ns ns-sym)
                                 (catch Throwable _ nil)))]
          (if in-jvm?
            (do (swap! state-atom update :loaded-macro-ns (fnil conj #{}) ns-sym)
                this)
            ;; 2. 用户 -L 路径：read-ns-source 支持目录 + jar，load-string 执行
            (if-let [src (resolve-lib/read-ns-source lib-paths ns-sym)]
              (do
                (load-string src)
                (swap! state-atom update :loaded-macro-ns (fnil conj #{}) ns-sym)
                this)
              (throw (ex-info (str "宏命名空间 " ns-sym " 在 -L 路径下找不到源文件")
                              {:ns ns-sym
                               :searched (resolve-lib/module-candidates lib-paths ns-sym)}))))))))

  (macro-ns-loaded? [_ ns-sym]
    (contains? (:loaded-macro-ns @state-atom #{}) ns-sym))
  (ns-exported-syms [this ns-sym]
    (ensure-compiled this ns-sym)
    (if-let [unit (proto/module-unit this ns-sym)]
      (mu/module-public-symbols unit)
      (throw (ex-info (str "依赖的 ns 未编译: " ns-sym)
                      {:ns ns-sym}))))
  (resolve-symbol-entry [this sym]
    (let [ns-part  (namespace sym)
          from-unit (when ns-part
                      (when-let [unit (proto/module-unit this (symbol ns-part))]
                        (get (mu/module-public-entries unit) sym)))
          builtin   (get (tp/builtin-symbols (proto/frontend this)) sym)]
      (or builtin from-unit ))))

(defn make-compile-context
  [config ^CompileTarget target]
  (let [init-state (->CompileContextState #{} {} {} #{})
        state-atom (atom init-state)]
    (->DefaultCompileContext config target state-atom)))
