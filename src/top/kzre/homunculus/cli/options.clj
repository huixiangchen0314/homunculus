(ns top.kzre.homunculus.cli.options
  (:require [clojure.string :as str]
            [top.kzre.homunculus.cli.option :as opt]))


(opt/reg-option
  ["-h" "--help"
   "显示帮助信息并退出"])

(opt/reg-option
  ["-v" "--version"
   "显示版本信息并退出"])

(opt/reg-option
  ["-o" "--output DIR"
   "输出路径"])

(opt/reg-option
  ["-t" "--target TARGET"
   "目标后端（如 hlsl, glsl）"])

(opt/reg-option
  ["-L" "--lib LIB"
   "添加库搜索路径（可重复）"
   :assoc-fn (fn [m k v] (update m k conj v))])

(opt/reg-option
  ["-s" "--style STYLE"
   "模块命名风格（:default, :flat, :flat-snake）"
   :parse-fn keyword])

(opt/reg-option
  ["-S" "--split-modules"
   "为每个模块生成独立的输出文件"])

(opt/reg-option
  ["-M" "--macro-ns NAMES"
   "添加宏命名空间,分号分隔多个。
   宏命名空间必须按 标准 clojure 点分路径在 -L 中可检索到。"
   :assoc-fn (fn [m k v]
               (update m k into
                       (map #(symbol %)
                            (str/split v #";"))))])

(opt/reg-option
  ["-c" "--config FILE" "指定配置文件路径（可用 ; 分隔多个，可重复传）"
   :assoc-fn (fn [m k v]
               (update m k into (remove str/blank? (str/split v #";"))))])