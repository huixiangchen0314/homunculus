(ns top.kzre.homunculus.core.ir2.core
  "IR2 核心：将 IR1 向量降低为语言无关的 IR2 AST。")

(defn make-node [kind & kvs]
  (apply assoc {:kind kind} kvs))

(defn literal      [val meta]   (make-node :literal :val val :meta meta))
(defn variable-ref [sym meta]   (make-node :variable :name (name sym) :meta meta))
(defn call-expr    [fn-expr args meta] (make-node :call :fn fn-expr :args (vec args) :meta meta))
(defn if-expr      [test then else meta] (make-node :if :test test :then then :else else :meta meta))
(defn block-expr   [exprs meta] (make-node :block :exprs (vec exprs) :meta meta))
(defn let-expr     [bindings body meta] (make-node :let :bindings (vec bindings) :body body :meta meta))
(defn loop-expr    [bindings body meta] (make-node :loop :bindings (vec bindings) :body body :meta meta))
(defn recur-expr   [args meta] (make-node :recur :args (vec args) :meta meta))
(defn lambda-expr  [params body captures fn-name meta]
  (make-node :lambda :params (vec params) :body body :captures (vec captures) :name fn-name :meta meta))
(defn define-expr  [name val doc attrs meta]
  (make-node :define :name (name name) :val val :doc doc :attrs attrs :meta meta))
(defn vector-expr  [items meta] (make-node :vector :items (vec items) :meta meta))
(defn map-expr     [kvs meta]   (make-node :map :kvs (vec kvs) :meta meta))
(defn throw-expr   [expr meta]  (make-node :throw :expr expr :meta meta))
(defn assign-expr  [var-sym val meta] (make-node :assign :var var-sym :val val :meta meta))
(defn try-expr     [body catches finally meta]
  (make-node :try :body (vec body) :catches (vec catches) :finally (vec (or finally [])) :meta meta))
(defn catch-expr   [class sym body] (make-node :catch :class class :sym sym :body (vec body)))

(defn ir1-meta [ir1-vec]
  (when-let [node (first ir1-vec)]
    (:meta node)))

(defmulti lower-ast
          (fn [ir1-vec env] (when (vector? ir1-vec) (:kind (first ir1-vec)))))

(defmethod lower-ast :literal [ir1-vec env]
  (let [node (first ir1-vec)]
    [(literal (:val node) (ir1-meta ir1-vec))]))

(defmethod lower-ast :symbol [ir1-vec env]
  (let [node (first ir1-vec)]
    [(variable-ref (:name node) (ir1-meta ir1-vec))]))

(defmethod lower-ast :vector [ir1-vec env]
  (let [items (mapv #(first (lower-ast % env)) (rest ir1-vec))]
    [(vector-expr items (ir1-meta ir1-vec))]))

(defmethod lower-ast :map [ir1-vec env]
  (let [pairs (:pairs (first ir1-vec))
        kvs   (mapcat (fn [[k v]] [(first (lower-ast k env)) (first (lower-ast v env))]) pairs)]
    [(map-expr kvs (ir1-meta ir1-vec))]))

(defmethod lower-ast :call [ir1-vec env]
  (let [op-ir (second ir1-vec) args-irs (nthrest ir1-vec 2)
        fn-node (first (lower-ast op-ir env))
        args    (mapv #(first (lower-ast % env)) args-irs)]
    [(call-expr fn-node args (ir1-meta ir1-vec))]))

;; 占位
(defmethod lower-ast :if [ir1-vec env] nil)
(defmethod lower-ast :do [ir1-vec env] nil)
(defmethod lower-ast :let* [ir1-vec env] nil)
(defmethod lower-ast :fn* [ir1-vec env] nil)
(defmethod lower-ast :def [ir1-vec env] nil)
(defmethod lower-ast :loop* [ir1-vec env] nil)
(defmethod lower-ast :recur [ir1-vec env] nil)
(defmethod lower-ast :quote [ir1-vec env] nil)
(defmethod lower-ast :try [ir1-vec env] nil)
(defmethod lower-ast :throw [ir1-vec env] nil)
(defmethod lower-ast :set! [ir1-vec env] nil)
(defmethod lower-ast :var [ir1-vec env] nil)

(defn lower [ir1-toplevel]
  (mapcat #(lower-ast % {}) ir1-toplevel))