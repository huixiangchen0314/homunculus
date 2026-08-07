(ns top.kzre.homunculus.internal.module-unit
  "模块编译单元：保存单个命名空间的编译中间结果。"
  (:require
    [top.kzre.homunculus.internal.symbol :as sym]))

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

(defn module-symbols [^ModuleUnit unit]
  (:symbol-table unit))

(defn module-public-symbols
  [^ModuleUnit unit]
  (into {} (filter #(not (sym/private-symbol? (val %))) unit)))

(defn module-requires [^ModuleUnit unit]
  (:requires unit))

(defn make-module-unit
  ([ns-sym]
   (make-module-unit ns-sym []))
  ([ns-sym nodes]
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

(defn lookup-symbol
  "尝试在模块中查找未限定符号表项"
  [^ModuleUnit unit sym]
  (let [normalized-sym (norm-sym unit sym)
        symbol-table (:symbol-table unit)]
    (get symbol-table normalized-sym)))