(ns top.kzre.homunculus.core.ir1
  "IR1 的目标是结构化的表达 Clojure 原语, 所有的IR1 节点均与
  Clojure 的形式对应."
  (:require [clojure.spec.alpha :as s]))

(s/def ::kind keyword?)

(defmulti parse-form
          "将 IR 节点 map 展开为 IR1 向量：[node child1 child2 ...]"
          (fn [ir1] (::kind ir1)))

(defn- make-node [kind & kvs]
  (apply assoc {::kind kind} kvs))

(declare parse-form)

(defn ->ir1
  "解析 Clojure 表单为 IR1 中间表示。"
  [form]
  (let [node
        (cond
          ;; 字面量
          (or (number? form) (string? form)
              (true? form) (false? form) (nil? form)
              (keyword? form) (char? form))
          (make-node :literal :val form)

          ;; 符号（保留元数据）
          (symbol? form)
          (make-node :symbol :name form :meta (meta form))

          ;; 列表 → 检查操作符
          (seq? form)
          (let [[op & args] form]
            (case op
              quote   (make-node :quote :expr (first args))
              if      (let [[test then else] args]
                        (make-node :if :test test :then then :else else))
              do      (make-node :do :exprs args)
              let*    (let [[bindings & body] args]
                        (make-node :let* :bindings bindings :body body))
              fn*     (let [[maybe-name params & body] args
                            [name params body] (if (symbol? maybe-name)
                                                 [maybe-name params body]
                                                 [nil maybe-name (cons params body)])]
                        (make-node :fn* :name name
                                   :params (mapv (fn [p] {:sym p :meta (meta p)}) params)
                                   :body body))
              def     (let [[sym & more] args
                            docstring? (when (string? (first more)) (first more))
                            rest-after-doc (if docstring? (rest more) more)
                            attr-map? (when (map? (first rest-after-doc)) (first rest-after-doc))
                            val-expr (if attr-map? (second rest-after-doc) (first rest-after-doc))]
                        (make-node :def :name sym
                                   :doc docstring?
                                   :attr attr-map?
                                   :val val-expr))
              loop*   (let [[bindings & body] args]
                        (make-node :loop :bindings bindings :body body))
              recur   (make-node :recur :exprs args)
              var     (make-node :var :var-sym (first args))
              throw   (make-node :throw :expr (first args))
              set!    (let [[var-sym val] args]
                        (make-node :set! :var var-sym :val val))
              try     (let [body-parts (take-while #(not (contains? #{'catch 'finally} (first %))) args)
                            after-body (drop (count body-parts) args)
                            catch-clauses (take-while #(= 'catch (first %)) after-body)
                            finally-part (drop (count catch-clauses) after-body)
                            finally-expr (when (= 'finally (ffirst finally-part))
                                           (rest (first finally-part)))]
                        (make-node :try :body body-parts
                                   :catches (mapv rest catch-clauses)
                                   :finally finally-expr))
              ;; 普通函数调用
              (make-node :call :op op :args args)))

          (vector? form)
          (make-node :vector :items form)

          (map? form)
          (make-node :map :pairs form)

          :else
          (throw (ex-info (str "Unsupported form: " form) {:form form})))]
    (parse-form node)))

;; ── 各节点解析方法 ──────────────────────────────
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
  (let [pairs (:pairs node)
        pair-irs (mapv ->ir1 (apply concat pairs))]
    (vec (cons node pair-irs))))

(defmethod parse-form :quote [node]
  (let [expr-ir (->ir1 (:expr node))]
    [node expr-ir]))

(defmethod parse-form :if [node]
  (let [test-ir (->ir1 (:test node))
        then-ir (->ir1 (:then node))
        else-ir (when (:else node) (->ir1 (:else node)))]
    (vec (remove nil? (list* node test-ir then-ir (when else-ir [else-ir]))))))

(defmethod parse-form :do [node]
  (let [expr-irs (mapv ->ir1 (:exprs node))]
    (vec (cons node expr-irs))))

(defmethod parse-form :let* [node]
  (let [bindings (:bindings node)
        pair-count (/ (count bindings) 2)
        binding-irs (mapv (fn [i]
                            (let [sym (nth bindings (* 2 i))
                                  val (nth bindings (inc (* 2 i)))]
                              [(->ir1 sym) (->ir1 val)]))
                          (range pair-count))
        body-irs (mapv ->ir1 (:body node))]
    (vec (cons node (concat (apply concat binding-irs) body-irs)))))

(defmethod parse-form :fn* [node]
  (let [params-vec (:params node)   ;; [{:sym s :meta m} ...]
        body (:body node)
        name (:name node)
        name-ir (when name (->ir1 name))
        ;; 解析参数符号（符号节点会携带元数据）
        param-irs (mapv #(->ir1 (:sym %)) params-vec)
        body-irs (mapv ->ir1 body)]
    (vec (cons node (remove nil? (concat (when name-ir [name-ir])
                                         param-irs
                                         body-irs))))))

(defmethod parse-form :def [node]
  (let [name-ir (->ir1 (:name node))
        doc-ir  (when-let [d (:doc node)] (->ir1 d))
        attr-ir (when-let [a (:attr node)] (->ir1 a))
        val-ir  (when-let [v (:val node)] (->ir1 v))]
    (vec (remove nil? (list* node name-ir doc-ir attr-ir (when val-ir [val-ir]))))))

(defmethod parse-form :loop [node]
  (let [bindings (:bindings node)
        pair-count (/ (count bindings) 2)
        binding-irs (mapv (fn [i]
                            (let [sym (nth bindings (* 2 i))
                                  val (nth bindings (inc (* 2 i)))]
                              [(->ir1 sym) (->ir1 val)]))
                          (range pair-count))
        body-irs (mapv ->ir1 (:body node))]
    (vec (cons node (concat (apply concat binding-irs) body-irs)))))

(defmethod parse-form :recur [node]
  (let [expr-irs (mapv ->ir1 (:exprs node))]
    (vec (cons node expr-irs))))

(defmethod parse-form :var [node]
  (let [sym-ir (->ir1 (:var-sym node))]
    [node sym-ir]))

(defmethod parse-form :throw [node]
  (let [expr-ir (->ir1 (:expr node))]
    [node expr-ir]))

(defmethod parse-form :set! [node]
  (let [var-ir (->ir1 (:var node))
        val-ir (->ir1 (:val node))]
    [node var-ir val-ir]))

(defmethod parse-form :try [node]
  (let [body-irs   (mapv ->ir1 (:body node))
        catch-irs  (mapv (fn [catch-clause]
                           (let [class-ir (->ir1 (first catch-clause))
                                 sym-ir   (->ir1 (second catch-clause))
                                 body-exprs (nthrest catch-clause 2)
                                 body-irs  (mapv ->ir1 body-exprs)]
                             (vec (cons (make-node :catch :class (:class class-ir) :sym (:sym sym-ir))
                                        (concat [class-ir sym-ir] body-irs)))))
                         (:catches node))
        finally-irs (when-let [fexpr (:finally node)]
                      (mapv ->ir1 fexpr))]
    (vec (cons node (concat body-irs catch-irs finally-irs)))))

(defmethod parse-form :catch [node]
  [node])