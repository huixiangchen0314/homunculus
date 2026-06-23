(ns top.kzre.homunculus.internal.utils
  "编译器内部工具函数"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (java.io FileNotFoundException PushbackReader StringReader)))

;; ── 符号转换工具 ────────────────────────
(defn ->symbol
  "将字符串、关键字、符号或 (quote sym) 形式统一转换为符号。
   若 x 是字符串或关键字，取其名称转换为符号；
   若 x 是符号，直接返回；
   若 x 是 (quote sym) 形式，提取 sym 并递归转换。
   其他类型返回 nil。"
  [x]
  (cond
    (symbol? x) x
    (string? x) (symbol x)
    (keyword? x) (symbol (name x))
    (and (seq? x) (= (first x) 'quote) (= (count x) 2))
    (->symbol (second x))
    :else nil))


(def ^:private module-naming-style-handlers
  {:default    (fn [ns-name suffix] (str (str/replace ns-name "." "/") suffix))
   :flat       (fn [ns-name suffix] (str ns-name suffix))
   :flat-snake (fn [ns-name suffix] (str (str/replace ns-name "." "_") suffix))})

(defn ns->module-path
  "根据 module-naming-style 将命名空间符号转换为模块文件名（包含可能的前缀路径，但不含输出根目录）。
   style : :default, :flat, :flat-snake (默认为 :default)
   suffix : 文件后缀，如 \".hlsl\"、\".glsl\"。"
  ([ns-sym suffix] (ns->module-path ns-sym :default suffix))
  ([ns-sym style suffix]
   (let [ns-name (name ns-sym)
         handler (get module-naming-style-handlers style (module-naming-style-handlers :default))]
     (handler ns-name suffix))))



(defn ns->path
  "将命名空间符号转换为相对于 base-path 的文件路径（.clj 扩展名）。"
  [base-path ns-sym]
  (let [ns-str         (name ns-sym)
        relative-path  (-> ns-str
                           (str/replace "." "/")
                           (str/replace "-" "_"))
        filename       (str relative-path ".clj")
        ;; 去掉 base-path 末尾可能存在的多余 /
        base           (if (str/ends-with? base-path "/")
                         (subs base-path 0 (dec (count base-path)))
                         base-path)]
    (str base "/" filename)))

(defn module-candidates
  "返回命名空间 ns-sym 在 lib-paths 下的所有候选文件路径（有序）。"
  [lib-paths ns-sym]
  (let [paths (if (coll? lib-paths) lib-paths [lib-paths])]
    (map #(ns->path % ns-sym) paths)))

(defn resolve-file
  "从 lib-paths 中查找命名空间 ns-sym 对应的第一个可读源文件，读取并返回其内容。
   找不到文件返回 nil。"
  [paths]
  (some (fn [path]
          (try (slurp path)
               (catch FileNotFoundException _ nil)))
        paths))

;; 新增：将命名空间符号转换为 classpath 资源路径
(defn- ns->resource-path [ns-sym]
  (-> (name ns-sym)
      (str/replace "." "/")
      (str/replace "-" "_")
      (str ".clj")))

(defn resolve-module
  "从 lib-paths 中查找模块源文件，若找不到则尝试从 classpath 资源加载（用于标准库等）。"
  [lib-paths ns-sym]
  (or (resolve-file (module-candidates lib-paths ns-sym))
      (when-let [res (io/resource (ns->resource-path ns-sym))]
        (slurp res))))


(defn parse-forms
  "从源文件字符串读取所有顶层表单，返回向量。"
  [src-str]
  (let [reader (PushbackReader. (StringReader. src-str))]
    (binding [*read-eval* false]
      (loop [forms []]
        (let [form (read reader false ::eof)]
          (if (= form ::eof)
            forms
            (recur (conj forms form))))))))


(defn detect-cycle
  "检测依赖图中是否存在循环依赖。使用深度优先搜索（DFS）。
   graph 格式：{module-a #{module-b module-c}, ...}
   若存在循环，返回循环路径上的模块列表；否则返回 nil。"
  [graph]
  (let [WHITE 0, GRAY 1, BLACK 2
        color (atom {})
        parent (atom {})
        cycle-start (atom nil)]
    (doseq [node (keys graph)]
      (swap! color assoc node WHITE))
    (letfn [(dfs [u]
              (swap! color assoc u GRAY)
              (doseq [v (get graph u #{})]
                (let [cv (@color v WHITE)]
                  (when (= cv GRAY)
                    (reset! cycle-start v)
                    (swap! parent assoc v u))
                  (when (= cv WHITE)
                    (swap! parent assoc v u)
                    (dfs v))))
              (swap! color assoc u BLACK))]
      (doseq [node (keys graph) :when (= (@color node WHITE) WHITE)]
        (dfs node)))
    (when @cycle-start
      (loop [path [] node @cycle-start]
        (if (contains? @parent node)
          (recur (conj path node) (@parent node))
          (conj path node))))))

(defn topological-sort
  "对依赖图进行拓扑排序，返回排序后的模块列表。
   若检测到循环依赖，抛出 ex-info 异常，包含循环路径。"
  [graph]
  (if-let [cycle (detect-cycle graph)]
    (throw (ex-info "Circular dependency detected" {:cycle cycle}))
    (let [in-degree (atom {})
          zero-queue (atom clojure.lang.PersistentQueue/EMPTY)
          sorted (atom [])]
      (doseq [node (keys graph)]
        (swap! in-degree assoc node 0))
      (doseq [[u deps] graph]
        (doseq [v deps]
          (swap! in-degree update v (fnil inc 0))))
      (doseq [[node deg] @in-degree :when (zero? deg)]
        (swap! zero-queue conj node))
      (while (seq @zero-queue)
        (let [u (peek @zero-queue)]
          (swap! zero-queue pop)
          (swap! sorted conj u)
          (doseq [v (get graph u #{})]
            (swap! in-degree update v dec)
            (when (zero? (@in-degree v))
              (swap! zero-queue conj v)))))
      (if (= (count @sorted) (count (keys graph)))
        @sorted
        (throw (ex-info "Circular dependency detected" {:graph graph}))))))