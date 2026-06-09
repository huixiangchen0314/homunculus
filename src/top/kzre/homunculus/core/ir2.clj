(ns top.kzre.homunculus.core.ir2
  (:require
    [clojure.spec.alpha :as s]
    [top.kzre.homunculus.core.ir1 :as ir1]))

(s/def ::kind keyword?)

(defn- make-node [kind & kvs]
  (if (seq kvs)
    (apply assoc {::kind kind} kvs)
    {::kind kind}))

(def ^:private prim-ops
  {'+        :add
   '-        :sub
   '*        :mul
   '/        :div
   'inc      :inc
   'dec      :dec
   'mod      :mod
   'rem      :rem
   '=        :eq
   '==       :eq
   'not=     :neq
   '<        :lt
   '<=       :lte
   '>        :gt
   '>=       :gte
   'and      :and
   'or       :or
   'not      :not
   'first    :first
   'second   :second
   'rest     :rest
   'nth      :nth
   'count    :count
   'conj     :conj
   'assoc    :assoc
   'dissoc   :dissoc
   'get      :get
   'str      :str
   'println  :print
   'prn      :print})

(defn lower [ir1-vec]
  (let [node (first ir1-vec)
        kind (::ir1/kind node)]
    (case kind
      :literal
      [(make-node :literal :val (:val node) :type (type (:val node)))]

      :symbol
      [(make-node :var :name (:name node))]

      :call
      (let [op-sym (:op node)
            prim-key (get prim-ops op-sym)]
        (if prim-key
          ;; 原语操作：子节点仅为参数（跳过操作符 ir1）
          (let [arg-irs (map lower (nthrest ir1-vec 2))]
            (vec (cons (make-node :prim :op prim-key) arg-irs)))
          ;; 普通调用：操作符作为第一个子节点
          (let [op-ir (lower (second ir1-vec))
                arg-irs (map lower (nthrest ir1-vec 2))]
            (vec (cons (make-node :call) (cons op-ir arg-irs))))))

      :if
      (let [test-ir (lower (second ir1-vec))
            then-ir (lower (nth ir1-vec 2))
            else-ir (when (>= (count ir1-vec) 4) (lower (nth ir1-vec 3)))]
        (vec (cons (make-node :if) (filter some? [test-ir then-ir else-ir]))))

      :do
      (let [expr-irs (map lower (rest ir1-vec))]
        (vec (cons (make-node :do) expr-irs)))

      :let*
      (let [bindings (:bindings node)
            bindings-count (/ (count bindings) 2)
            all-children (rest ir1-vec)
            bind-irs (take (* 2 bindings-count) all-children)
            body-irs (drop (* 2 bindings-count) all-children)
            sym-ir2s (map #(lower %) (take-nth 2 bind-irs))
            val-ir2s (map #(lower %) (take-nth 2 (rest bind-irs)))
            body-ir2s (map lower body-irs)]
        (vec (cons (make-node :let) (concat (interleave sym-ir2s val-ir2s) body-ir2s))))

      :fn*
      (let [params-vec (:params node)
            param-count (count params-vec)
            children (rest ir1-vec)
            ;; 是否有名称？
            has-name (boolean (:name node))
            name-ir2 (when has-name (lower (first children)))
            param-start (if has-name 1 0)
            param-ir1s (take param-count (drop param-start children))
            body-start (+ param-start param-count)
            body-ir1s (drop body-start children)
            param-ir2s (map lower param-ir1s)
            body-ir2s (map lower body-ir1s)]
        (vec (cons (make-node :fn)
                   (concat (when name-ir2 [name-ir2])
                           param-ir2s
                           body-ir2s))))

      :def
      (let [name-ir (lower (second ir1-vec))
            val-ir (when (>= (count ir1-vec) 3) (lower (nth ir1-vec 2)))]
        (vec (cons (make-node :def) (filter some? [name-ir val-ir]))))

      :loop
      (let [bindings (:bindings node)
            bindings-count (/ (count bindings) 2)
            all-children (rest ir1-vec)
            bind-irs (take (* 2 bindings-count) all-children)
            body-irs (drop (* 2 bindings-count) all-children)
            sym-ir2s (map #(lower %) (take-nth 2 bind-irs))
            val-ir2s (map #(lower %) (take-nth 2 (rest bind-irs)))
            body-ir2s (map lower body-irs)]
        (vec (cons (make-node :loop) (concat (interleave sym-ir2s val-ir2s) body-ir2s))))

      :recur
      (let [expr-irs (map lower (rest ir1-vec))]
        (vec (cons (make-node :recur) expr-irs)))

      :quote
      [(make-node :quote) (lower (second ir1-vec))]

      ;; 其他未专门 lowered 的节点，递归降低子节点并保留 ::kind
      (let [child-ir2s (map lower (rest ir1-vec))]
        (vec (cons (assoc node ::kind kind) child-ir2s))))))