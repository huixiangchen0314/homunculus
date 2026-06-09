(ns top.kzre.homunculus.core.ir2
  (:require
    [clojure.spec.alpha :as s]
    [top.kzre.homunculus.core.ir1 :as ir1]))

(s/def ::kind keyword?)

(defn- make-node
  "创建 IR2 节点 map，至少包含 ::kind。"
  [kind & kvs]
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

(defn lower
  "将 IR1 向量降级为 IR2 向量。"
  [ir1-vec]
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
          ;; 原语操作，子节点仅为参数（跳过操作符 ir1）
          (let [arg-irs (map lower (nthrest ir1-vec 2))]
            (vec (cons (make-node :prim :op prim-key) arg-irs)))
          ;; 普通调用
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
            bindings-count (count bindings)          ;; 元素总数
            all-children (rest ir1-vec)
            bind-irs (take bindings-count all-children)
            body-irs (drop bindings-count all-children)
            sym-ir2s (map #(lower %) (take-nth 2 bind-irs))
            val-ir2s (map #(lower %) (take-nth 2 (rest bind-irs)))
            body-ir2s (map lower body-irs)]
        (vec (cons (make-node :let :bindings-count bindings-count)
                   (concat (interleave sym-ir2s val-ir2s) body-ir2s))))

      :fn*
      (let [params-vec (:params node)
            fn-name (:name node)
            has-name (boolean fn-name)
            children (rest ir1-vec)
            name-ir2 (when has-name (lower (first children)))
            param-start (if has-name 1 0)
            param-ir1s (take (count params-vec) (drop param-start children))
            body-ir1s (drop (+ param-start (count params-vec)) children)
            param-ir2s (map lower param-ir1s)
            body-ir2s (map lower body-ir1s)]
        (vec (cons (make-node :fn ::params params-vec ::fn-name fn-name)
                   (concat (when name-ir2 [name-ir2]) param-ir2s body-ir2s))))

      :def
      (let [name-sym (:name node)                       ;; def 的名字符号
            name-ir (lower (second ir1-vec))            ;; 符号的 IR2
            raw-val (when (>= (count ir1-vec) 3)        ;; 值可能不存在
                      (lower (nth ir1-vec 2)))]
        ;; 如果值是一个 :fn 节点，就把 def 的名字附加上去
        (let [val-ir (if (and raw-val (= (::kind (first raw-val)) :fn))
                       (let [new-node (assoc (first raw-val) ::fn-name name-sym)]
                         (vec (cons new-node (rest raw-val))))
                       raw-val)]
          (vec (cons (make-node :def) (filter some? [name-ir val-ir])))))

      :loop
      (let [bindings (:bindings node)
            bindings-count (count bindings)
            all-children (rest ir1-vec)
            bind-irs (take bindings-count all-children)
            body-irs (drop bindings-count all-children)
            sym-ir2s (map #(lower %) (take-nth 2 bind-irs))
            val-ir2s (map #(lower %) (take-nth 2 (rest bind-irs)))
            body-ir2s (map lower body-irs)]
        (vec (cons (make-node :loop :bindings-count bindings-count)
                   (concat (interleave sym-ir2s val-ir2s) body-ir2s))))

      :recur
      (let [expr-irs (map lower (rest ir1-vec))]
        (vec (cons (make-node :recur) expr-irs)))

      :quote
      [(make-node :quote) (lower (second ir1-vec))]

      ;; 其他未专门 lowered 的节点
      (let [child-ir2s (map lower (rest ir1-vec))]
        (vec (cons (assoc node ::kind kind) child-ir2s))))))