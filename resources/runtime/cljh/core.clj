(ns cljh.core
  "cljh 运行时标准库。仅依赖于 cljh.lang.RT 中的动态操作。
   所有桥接函数标记 :inline，编译器在调用点直接展开为原生运行时调用。
   类型转换、简单谓词等静态可确定的操作不在运行时桥接，由编译器直接处理。")

;; 序列核心（多态，必须运行时）
(defn ^:inline cons [x seq]
  (. cljh.lang.RT cons x seq))

(defn ^:inline first [coll]
  (. cljh.lang.RT first coll))

(defn ^:inline rest [coll]
  (. cljh.lang.RT rest coll))

(defn ^:inline next [coll]
  (. cljh.lang.RT next coll))

(defn ^:inline seq [coll]
  (. cljh.lang.RT seq coll))

(defn ^:inline conj [coll x]
  (. cljh.lang.RT conj coll x))

;; 集合构造（需要运行时分配结构）
(defn ^:inline vector [& items]
  (. cljh.lang.RT vector items))

(defn ^:inline hash-map [& kvs]
  (. cljh.lang.RT hashMap kvs))

(defn ^:inline set [& items]
  (. cljh.lang.RT set items))

(defn ^:inline list [& items]
  (. cljh.lang.RT list items))

;; 映射 / 集合查询（数据结构多态）
(defn ^:inline get
  ([coll key]            (. cljh.lang.RT get coll key))
  ([coll key not-found]  (. cljh.lang.RT get coll key not-found)))

(defn ^:inline assoc [coll key val]
  (. cljh.lang.RT assoc coll key val))

(defn ^:inline dissoc [coll key]
  (. cljh.lang.RT dissoc coll key))

(defn ^:inline contains? [coll key]
  (. cljh.lang.RT contains? coll key))

(defn ^:inline count [coll]
  (. cljh.lang.RT count coll))

(defn ^:inline nth
  ([coll idx]            (. cljh.lang.RT nth coll idx))
  ([coll idx not-found]  (. cljh.lang.RT nth coll idx not-found)))

;; 相等与比较（值多态，需运行时）
(defn ^:inline = [x y]
  (. cljh.lang.RT equals x y))

(defn ^:inline compare [x y]
  (. cljh.lang.RT compare x y))

;; 元数据（运行时刻附加/读取）
(defn ^:inline meta [obj]
  (. cljh.lang.RT meta obj))

(defn ^:inline with-meta [obj m]
  (. cljh.lang.RT withMeta obj m))


;; 函数应用（动态参数列表展开）
(defn ^:inline apply [f & args]
  (. cljh.lang.RT applyTo f args))

;; 惰性序列（运行时的延迟求值）
(defn ^:inline lazy-seq* [thunk]
  (. cljh.lang.RT lazySeq thunk))

(defmacro lazy-seq [& body]
  `(lazy-seq* (fn [] ~@body)))
