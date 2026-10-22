(ns top.kzre.homunculus.cli.option
  "可扩展的命令行选项解析。

   参照 LLVM / Git 风格，支持多个后端动态注册选项。
   每个选项规范直接沿用 clojure.tools.cli 的风格：
     [短选项 长选项 说明 & 关键字参数]

   其中长选项字符串需按 tools.cli 约定书写：
     - 需要参数的选项，在长选项后跟参数名，如 \"--target TARGET\"
     - flag 选项不带参数名，如 \"--help\"
   关键字参数同样直接透传给 tools.cli
   （:default / :parse-fn / :assoc-fn / :validate / :required 等）。"
  (:require [clojure.string :as str]
            [clojure.tools.cli :as cli]))

;; ═══════════════════════════════════════════════════════════
;; 全局选项注册表
;; ═══════════════════════════════════════════════════════════

(def ^:private option-registry
         "注册表：atom of vector，元素是 tools.cli 风格的选项规范。"
         (atom []))

(defn- long-name
  "从长选项字符串中取选项名部分（去掉参数名）。
   例：\"--target TARGET\" -> \"--target\"，\"--help\" -> \"--help\"。"
  [long]
  (first (str/split long #"\s+")))

(defn reg-option
  "注册一个命令行选项，规范与 clojure.tools.cli 完全一致。

   参数：
     opt-spec - [short long desc & {:as opts}]
                short 必须以 '-' 开头，long 必须以 '--' 开头。
                需要参数的选项在 long 后写参数名，如 \"--target TARGET\"；
                flag 选项不写参数名，如 \"--help\"。

   行为：
     - 短选项或长选项（比较时忽略参数名）已注册则抛 ex-info。"
  [opt-spec]
  (let [[short long] opt-spec]
    (when-not (and (string? short) (str/starts-with? short "-"))
      (throw (ex-info "短选项必须以 - 开头" {:spec opt-spec})))
    (when-not (and (string? long) (str/starts-with? long "--"))
      (throw (ex-info "长选项必须以 -- 开头" {:spec opt-spec})))
    (swap! option-registry
           (fn [reg]
             (when (some #(= (first %) short) reg)
               (throw (ex-info (str "短选项 " short " 已注册") {:short short})))
             (when (some #(= (long-name (second %)) (long-name long)) reg)
               (throw (ex-info (str "长选项 " (long-name long) " 已注册")
                               {:long (long-name long)})))
             (conj reg opt-spec)))))

(defn clear-options!
  "清空所有已注册的选项（主要用于测试）。"
  []
  (reset! option-registry []))

;; ═══════════════════════════════════════════════════════════
;; 解析主函数
;; ═══════════════════════════════════════════════════════════

(defn parse-opts
  "解析命令行参数 args（字符串序列）。
   返回 {:options map, :files [], :errors []}。"
  [args]
  (let [{:keys [options arguments errors]}
        (cli/parse-opts args @option-registry)]
    {:options options
     :files   arguments
     :errors  errors}))

;; ═══════════════════════════════════════════════════════════
;; 帮助信息（保留原来的排版风格）
;; ═══════════════════════════════════════════════════════════

(defn- split-long-spec
  "把 \"--target TARGET\" 拆成 [\"--target\" \"TARGET\"]，
   \"--help\" 拆成 [\"--help\" nil]。"
  [long]
  (let [[name arg] (str/split long #"\s+" 2)]
    [name arg]))

(defn usage-string
  "根据当前注册的选项生成帮助信息。"
  []
  (str "用法: homunculus [选项] <输入文件...>\n\n"
       "选项:\n"
       (str/join
         "\n"
         (for [[short long desc & more] @option-registry
               :let [opts (apply hash-map more)
                     default (:default opts)
                     [long-name arg-name] (split-long-spec long)]]
           (format "  %-4s %-20s %s%s"
                   short
                   (str long-name (when arg-name (str " <" arg-name ">")))
                   desc
                   (if (some? default)
                     (str " (默认: " default ")")
                     ""))))))

