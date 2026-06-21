(ns top.kzre.homunculus.core.ir1.expand-symbols
  "符号展开：根据 ns 声明将符号转换为全限定名，辅助宏展开。
   修复：对 :refer :all 的命名空间，通过 require + resolve 进行逐个尝试，
   避免依赖 find-ns 导致的加载失败。")

(defn- parse-ns [ns-form]
  (let [rest-args (drop 2 ns-form)
        require-clause (some #(when (and (sequential? %) (= :require (first %))) %)
                             (filter sequential? rest-args))
        aliases   (atom {})
        refers    (atom {})
        refer-all (atom #{})]
    (when require-clause
      (doseq [spec (rest require-clause)]
        (let [spec (if (sequential? spec) (vec spec) spec)]
          (cond
            (symbol? spec)
            (swap! refers assoc spec spec)

            (and (vector? spec) (= 3 (count spec)) (= :as (second spec)))
            (let [full-ns (first spec)
                  alias   (nth spec 2)]
              (swap! aliases assoc alias full-ns))

            (and (vector? spec) (= 3 (count spec)) (= :refer (second spec)) (= :all (nth spec 2)))
            (let [full-ns (first spec)]
              (swap! refer-all conj full-ns))

            (and (vector? spec) (= :refer (second spec)))
            (let [full-ns (first spec)
                  syms (nth spec 2)]
              (doseq [s syms]
                (swap! refers assoc s full-ns)))))))
    {:aliases @aliases :refers @refers :refer-all @refer-all}))

(defn- qualify-symbol [sym {:keys [aliases refers refer-all]}]
  (if (namespace sym)
    sym
    (or (get aliases sym)
        (get refers sym)
        (when (seq refer-all)
          (some (fn [ns-sym]
                  (try
                    (require ns-sym)
                    (when (resolve (symbol (str ns-sym) (name sym)))
                      (symbol (str ns-sym) (name sym)))
                    (catch Exception _ nil)))
                refer-all))
        ;; 回退：尝试从编译器核心命名空间解析（如 defn, defn-）
        (when-let [core-ns 'top.kzre.homunculus.core]
          (try
            (require core-ns)
            (when (resolve (symbol (str core-ns) (name sym)))
              (symbol (str core-ns) (name sym)))
            (catch Exception _ nil)))
        (throw (ex-info (str "Cannot qualify symbol: " sym)
                        {:sym sym :aliases aliases :refers refers :refer-all refer-all})))))

(defn resolve-ns
  "解析 ns 形式, 解析成功返回结果，否则返回nil"
  [ns-form]
  (parse-ns ns-form))


(defn expand-sym [sym ns-info]
  (qualify-symbol sym ns-info))