(ns top.kzre.homunculus.cli.options
  "可扩展的命令行选项解析。
   参照 LLVM / Git 风格，支持多个后端动态注册选项。
   每个选项规范形如 [短选项 长选项 说明 :default 默认值 :parse-fn 解析函数 :assoc-fn 累加函数]"
  (:require [clojure.string :as str]))

;; ═══════════════════════════════════════════════════════════
;; 全局选项注册表
;; ═══════════════════════════════════════════════════════════

(defonce ^:private option-registry
         (atom []))

(defn- find-option-by-short [short-opt]
  (first (filter #(= (first %) short-opt) @option-registry)))

(defn- find-option-by-long [long-opt]
  (first (filter #(= (second %) long-opt) @option-registry)))

(defn register-option!
  "注册一个命令行选项。参数格式：
   [short-opt long-opt description & {:keys [default parse-fn assoc-fn]}]。
   short-opt 必须以 '-' 开头，long-opt 必须以 '--' 开头。
   若短选项或长选项已注册，抛出异常。"
  [opt-spec]
  (let [[short long desc & {:as opts}] opt-spec]
    (when-not (str/starts-with? short "-")
      (throw (ex-info "短选项必须以 - 开头" {:spec opt-spec})))
    (when-not (str/starts-with? long "--")
      (throw (ex-info "长选项必须以 -- 开头" {:spec opt-spec})))
    (swap! option-registry
           (fn [reg]
             (when (find-option-by-short short)
               (throw (ex-info (str "短选项 " short " 已注册") {})))
             (when (find-option-by-long long)
               (throw (ex-info (str "长选项 " long " 已注册") {})))
             (conj reg opt-spec)))))

(defn clear-options!
  "清空所有已注册的选项（主要用于测试）。"
  []
  (reset! option-registry []))

;; ═══════════════════════════════════════════════════════════
;; 基础选项注册（内置）
;; ═══════════════════════════════════════════════════════════

;; 修正后的选项注册
(register-option!
  ["-h" "--help" "显示帮助信息并退出"
   :default false])

(register-option!
  ["-v" "--version" "显示版本信息并退出"
   :default false])

(register-option!
  ["-o" "--output" "输出目录"
   :default "out"])

(register-option!
  ["-t" "--target" "目标平台（如 hlsl, glsl）"
   :default "hlsl"])

(register-option!
  ["-I" "--include" "添加 include 搜索路径（可重复）"
   :default []
   :assoc-fn (fn [m k v] (update m k conj v))])

(register-option!
  ["-L" "--lib" "添加库搜索路径（可重复）"
   :default []
   :assoc-fn (fn [m k v] (update m k conj v))])

(register-option!
  ["-s" "--style" "模块命名风格（:default, :flat, :flat-snake）"
   :default :default
   :parse-fn keyword])

(register-option!
  ["-S" "--split-modules" "为每个模块生成独立的输出文件"
   :default false])

;; ═══════════════════════════════════════════════════════════
;; 选项映射工具
;; ═══════════════════════════════════════════════════════════

(defn- opt->key
  "根据短选项或长选项字符串，返回对应的关键字键。"
  [opt-str]
  (when-let [spec (or (find-option-by-short opt-str)
                      (find-option-by-long opt-str))]
    (keyword (str/replace (second spec) #"^--" ""))))   ;; e.g. "--output" -> :output

(defn- takes-arg?
  "判断选项是否需要参数（非布尔值）。"
  [opt-str]
  (when-let [spec (or (find-option-by-short opt-str)
                      (find-option-by-long opt-str))]
    (not (true? (:default (apply hash-map (drop 3 spec))))))) ; 简单启发：默认值为 true 则是标志

(defn- default-options-map []
  (reduce (fn [m spec]
            (let [k (opt->key (first spec))
                  default (get (apply hash-map (drop 3 spec)) :default)]
              (assoc m k default)))
          {}
          @option-registry))

;; ═══════════════════════════════════════════════════════════
;; 解析主函数
;; ═══════════════════════════════════════════════════════════

(defn parse-opts
  "解析命令行参数 args（字符串序列）。
   返回 {:options map, :files [], :errors []}。
   使用当前全局注册的选项定义。"
  [args]
  (let [opts-map (default-options-map)]
    (loop [args   (vec args)
           opts   opts-map
           files  []
           errors []]
      (if (empty? args)
        {:options opts :files files :errors errors}
        (let [arg (first args)]
          (cond
            (= arg "--")
            (recur [] opts (into files (rest args)) errors)

            (or (str/starts-with? arg "--") (str/starts-with? arg "-"))
            (if-let [key (opt->key arg)]
              (if (takes-arg? arg)
                (if (next args)
                  (let [val (second args)
                        spec (or (find-option-by-short arg)
                                 (find-option-by-long arg))
                        {:keys [parse-fn assoc-fn]} (apply hash-map (drop 3 spec))
                        new-val (if parse-fn (parse-fn val) val)]
                    (recur (drop 2 args)
                           (if assoc-fn
                             (assoc-fn opts key new-val)
                             (assoc opts key new-val))
                           files errors))
                  (recur (rest args) opts files
                         (conj errors (str "选项 " arg " 需要参数"))))
                ;; 标志选项
                (recur (rest args) (assoc opts key true) files errors))
              ;; 未知选项
              (recur (rest args) opts files (conj errors (str "未知选项: " arg))))

            ;; 位置参数（文件）
            :else
            (recur (rest args) opts (conj files arg) errors)))))))

;; ═══════════════════════════════════════════════════════════
;; 动态帮助信息
;; ═══════════════════════════════════════════════════════════

(defn usage-string
  "根据当前注册的选项生成帮助信息。"
  []
  (str "用法: homunculus [选项] <输入文件...>\n\n"
       "选项:\n"
       (str/join "\n"
                 (for [[short long desc] @option-registry]
                   (let [arg-name (when (takes-arg? short)
                                    (str " <" (str/replace (str/upper-case (str/replace long #"--" "")) #"-" "_") ">"))
                         default (get (apply hash-map (drop 3 (or (find-option-by-short short)
                                                                  (find-option-by-long long)))) :default)]
                     (format "  %-4s %-20s %s%s"
                             short
                             (str long (or arg-name ""))
                             desc
                             (if (some? default)
                               (str " (默认: " default ")")
                               "")))))))

;; ═══════════════════════════════════════════════════════════
;; 示例（在 REPL 中测试）
;; ═══════════════════════════════════════════════════════════
(comment
  (clear-options!)
  ;; 重新注册基础选项（已内置，但可调用）
  (parse-opts ["-o" "build" "-I" "inc1" "-I" "inc2" "file.clj"])
  ;; => {:options {:help false, :version false, :output "build", :target "hlsl", :include ["inc1" "inc2"], :lib [], :style :default}
  ;;     :files ["file.clj"], :errors []}

  ;; 后端添加自定义选项
  (register-option! ["-e" "--entry NAME" "入口函数名" :default "main"])
  (parse-opts ["-e" "vsMain" "file.clj"])
  ;; => {:options {..., :entry "vsMain"}, :files ["file.clj"], :errors []}
  )