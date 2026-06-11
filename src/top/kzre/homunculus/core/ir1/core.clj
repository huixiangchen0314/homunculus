(ns top.kzre.homunculus.core.ir1.core
  "IR1 核心：节点构造器、基本类型分发、入口函数。
   所有特殊形式的解析逻辑在 ir1.forms 中。"
  )

(defn make-node [kind & kvs]
  (apply assoc {:kind kind} kvs))

(declare ->ir1)

(defmulti form->node
          (fn [form]
            (cond
              (or (number? form) (string? form)
                  (true? form) (false? form) (nil? form)
                  (keyword? form) (char? form))
              :literal
              (symbol? form) :symbol
              (vector? form) :vector
              (map? form)    :map
              (seq? form)    (if (symbol? (first form))
                               (first form)
                               :call)
              :else (throw (ex-info (str "Unsupported form: " form) {:form form})))))

;; ── 基础类型方法 ─────────────────────────────
(defmethod form->node :literal [form] (make-node :literal :val form))
(defmethod form->node :symbol  [form] (make-node :symbol :name form :meta (meta form)))
(defmethod form->node :vector  [form] (make-node :vector :items form))
(defmethod form->node :map     [form] (make-node :map :pairs form))

;; 处理第一个元素不是符号的列表（如 (1 2)）
(defmethod form->node :call [form]
  (let [[op & args] form]
    (make-node :call :op op :args args)))

;; ── 新增：默认方法，处理所有未被特殊形式匹配的列表 ──
(defmethod form->node :default [form]
  (if (seq? form)
    (let [[op & args] form]
      (make-node :call :op op :args args))
    (throw (ex-info (str "Unsupported form: " form) {:form form}))))

;; ── parse-form 及其方法保持不变 ─────────────
(defmulti parse-form
          (fn [node] (::kind node)))

(defmethod parse-form :literal [node] [node])
(defmethod parse-form :symbol  [node] [node])

(defmethod parse-form :call [node]
  (let [op-ir   (->ir1 (:op node))
        arg-irs (mapv ->ir1 (:args node))]
    (vec (cons node (cons op-ir arg-irs)))))

(defmethod parse-form :vector [node]
  (let [item-irs (mapv ->ir1 (:items node))]
    (vec (cons node item-irs))))

(defmethod parse-form :map [node]
  (let [pair-irs (mapv ->ir1 (apply concat (:pairs node)))]
    (vec (cons node pair-irs))))

(defn ->ir1 [form]
  (parse-form (form->node form)))