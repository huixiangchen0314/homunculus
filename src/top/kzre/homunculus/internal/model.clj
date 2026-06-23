(ns top.kzre.homunculus.internal.model
  "编译配置和上下文的默认实现。"
  (:require
    [clojure.spec.alpha :as s]
    [top.kzre.homunculus.internal.protocol :as p]
    [top.kzre.homunculus.internal.spec :as spec]
    [top.kzre.homunculus.internal.utils :as u]))

(defn get-module-unit [context ns-sym]
  (get-in @(:state context) [:modules ns-sym]))

(defn set-module-unit! [context ns-sym unit]
  (swap! (:state context) assoc-in [:modules ns-sym] unit))


(defrecord CompileConfig [source-paths lib-paths output-dir]
  p/ICompileConfig
  (source-paths [_] source-paths)
  (lib-paths [_] lib-paths)
  (output-dir [_] output-dir)
  (module-naming-style [_] :default))


(defn- ensure-compiled [ctx ns-sym]
  (when-not (get-module-unit ctx ns-sym)
    (let [state (:state ctx)]
      (when (contains? (get @state :compiling) ns-sym)
        (throw (ex-info "Circular dependency" {:module ns-sym})))
      (swap! state update :compiling conj ns-sym)
      (try
        (p/compile-module (:compiler ctx) ns-sym ctx)
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

