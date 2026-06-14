(ns top.kzre.homunculus.internal.model
  "编译配置和上下文的默认实现。"
  (:require [top.kzre.homunculus.internal.protocol :as p]
            [top.kzre.homunculus.internal.utils :as utils]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (clojure.lang LineNumberingPushbackReader)
           (java.io StringReader)))

;; ── 默认配置 ──
(defrecord DefaultCompileConfig [source-paths lib-paths output-dir]
  p/ICompileConfig
  (source-paths [_] source-paths)
  (lib-paths [_] lib-paths)
  (output-dir [_] output-dir))

;; ── 默认文件解析器 ──
(defrecord DefaultFileResolver [source-paths]
  p/IFileResolver
  (resolve-file [_ ns-sym]
    (let [rel-path (-> (name ns-sym)
                       (str/replace "." "/")
                       (str ".clj"))
          files (mapcat #(let [f (io/file % rel-path)]
                           (when (.exists f) [f]))
                        source-paths)]
      (first files)))
  (read-content [_ file]
    (slurp file)))

;; ── 默认编译上下文 ──
;; 包含一个 state atom，用于存储编译状态和缓存。
(defrecord DefaultCompileContext [config file-resolver backend state]
  p/ICompileContext
  (config [_] config)

  ;; resolve-ns：加载源文件表单
  (resolve-ns [_ ns-sym]
    (when-let [file (p/resolve-file file-resolver ns-sym)]
      (p/read-content file-resolver file)))

  ;; register-deps：注册依赖并递归编译，利用 state 进行缓存和循环检测
  (register-deps [this dep-syms]
    (let [compile-module
          (fn compile-module [ns-sym]
            ;; 检查是否正在编译中，预防循环依赖
            (when (contains? (get @state :compiling #{}) ns-sym)
              (throw (ex-info "Circular dependency detected"
                              {:module ns-sym
                               :compiling (get @state :compiling)})))
            ;; 如果尚未缓存，则编译
            (when-not (get-in @state [:modules ns-sym])
              (swap! state update :compiling conj ns-sym)
              (try
                (let [forms (p/resolve-ns this ns-sym)]
                  (when-not forms
                    (throw (ex-info "Module not found" {:module ns-sym})))
                  ;; 使用后端编译该模块，获得编译结果（类型信息等）
                  (let [result (p/compile backend forms this)]
                    ;; 将编译结果存入 state，同时记录表单和结果
                    (swap! state assoc-in [:modules ns-sym]
                           {:forms forms :result result})))
                (finally
                  (swap! state update :compiling disj ns-sym)))))]
      (doseq [dep dep-syms]
        (compile-module dep))))

  ;; lookup-type：根据完全限定名查找类型（假设编译结果中包含 :exports 映射）
  (lookup-type [_ full-name]
    (let [ns (namespace full-name)
          sym (name full-name)]
      (when-let [exports (get-in @state [:modules (symbol ns) :result :exports])]
        (get exports (symbol sym)))))

  ;; get-export-syms：获取某命名空间的导出符号表
  (get-export-syms [_ ns-sym]
    (get-in @state [:modules ns-sym :result :exports])))

;; 构造函数：提供默认 state atom，方便外部使用
(defn ->default-compile-context [config file-resolver backend]
  (->DefaultCompileContext config file-resolver backend
                           (atom {:modules {} :compiling #{}})))