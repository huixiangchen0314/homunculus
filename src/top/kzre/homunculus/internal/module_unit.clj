(ns top.kzre.homunculus.internal.module-unit
  "模块编译单元：保存单个命名空间的编译中间结果。"
  (:require
    [top.kzre.homunculus.core.symbol :as sym]))

(defrecord ModuleUnit
  [ns-sym          ;; 命名空间符号
   nodes       ;; 经过约束求解（solve）之后的 IR2 根节点列表
   symbol-table    ;; 模块级符号表
   requires        ;; 导入模块 ns-sym 集合
   ])

(defn module-ns [^ModuleUnit unit]
  (:ns-sym unit))

(defn module-nodes [^ModuleUnit unit]
  (:nodes unit))

(defn module-public-entries
  "模块公开的符号表项。"
  [^ModuleUnit unit]
  (into {}
        (filter #(not (sym/private-symbol? (val %)))
                (:symbol-table unit))))

(defn module-public-symbols
  "模块导出的可见符号"
  [^ModuleUnit unit]
  (let [entries (into {} (filter #(not (sym/private-symbol? (val %))) unit))
        table (:symbol-table entries)]
    (set (keys table))))

(defn make-module-unit
  ([ns-sym]
   (make-module-unit ns-sym []))
  ([ns-sym nodes]
   {:pre [(some? ns-sym)]}
   (->ModuleUnit ns-sym nodes {} #{})))

(defn norm-sym
  "用模块命名空间规范符号"
  [^ModuleUnit unit sym]
  (let [ns-sym (module-ns unit)
        normalized-sym (symbol (name ns-sym) (name sym))]
    normalized-sym))

(defn register-symbol
  "注册符号表，规范符号"
  [^ModuleUnit unit entry]
  (let [sym (:sym entry)
        normalized-sym (norm-sym unit sym)
        normalized-entry (assoc entry :sym normalized-sym)]
    (assoc-in unit [:symbol-table normalized-sym] normalized-entry)))
