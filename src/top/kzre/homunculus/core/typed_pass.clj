(ns top.kzre.homunculus.core.typed-pass
  "对IR2进行解析，进行类型推导，在IR2节点上附加类型信息."
  (:require [top.kzre.homunculus.core.ir2 :as ir2]
            [clojure.string :as str]))

;; ═══════════════════════════════════════════════
;; Protobuf 风格类型定义（语言无关）
;; ═══════════════════════════════════════════════

(defn prim-type [name] {:kind :prim, :name name})
(def T-int    (prim-type :int))
(def T-float  (prim-type :float))
(def T-double (prim-type :double))
(def T-bool   (prim-type :bool))
(def T-string (prim-type :string))
(def T-bytes  (prim-type :bytes))
(def T-void   (prim-type :void))
(def T-any    {:kind :any})
(def T-unknown {:kind :unknown})

(defn vector-type [elem] {:kind :vector, :elem-type elem})
(defn map-type [k v] {:kind :map, :key-type k, :val-type v})
(defn fn-type [params ret] {:kind :function, :param-types params, :return-type ret})
(defn optional-type [inner] {:kind :optional, :inner-type inner})
(defn struct-type [fields] {:kind :struct, :fields fields})
(defn class-type [name fields & {:keys [parent implements]}]
  {:kind :class, :name name, :fields fields, :parent parent, :implements implements})
(defn enum-type [values] {:kind :enum, :values values})
(defn oneof-type [options] {:kind :oneof, :options options})
(defn pointer-type [pointee & {:keys [const]}] {:kind :pointer, :pointee pointee, :const (boolean const)})
(defn named-type [name & type-args]
  (cond-> {:kind :named, :name name}
          (seq type-args) (assoc :type-args (vec type-args))))

;; ── 字面量类型 ─────────────────────────────────
(defn literal-type [v]
  (cond
    (integer? v) T-int
    (float? v) T-float
    (ratio? v) T-float
    (double? v) T-double
    (string? v) T-string
    (true? v) T-bool
    (false? v) T-bool
    (nil? v) T-void
    (keyword? v) T-string
    :else T-any))

;; ── 原语操作返回类型推导 ──────────────────────
(defn infer-prim-return [op [arg1 arg2]]
  (case op
    (:add :sub :mul :div) (if (or (= (:kind arg1) :float) (= (:kind arg2) :float)) T-float arg1)
    (:eq :neq :lt :lte :gt :gte :and :or :not) T-bool
    :first  (if (= (:kind arg1) :vector) (:elem-type arg1) T-unknown)
    :second (if (= (:kind arg1) :vector) (:elem-type arg1) T-unknown)
    :nth    (if (= (:kind arg1) :vector) (:elem-type arg1) T-unknown)
    :count  T-int
    :get    (if (= (:kind arg1) :map) (:val-type arg1) T-unknown)
    T-any))

;; ── 类型推导辅助 ──────────────────────────────
(defn- guess-param-type [sym]
  (let [n (str/lower-case (name sym))]
    (cond
      (re-find #"normal|dir|light|color" n) (vector-type T-float)
      (re-find #"position|pos" n) (vector-type T-float)
      (re-find #"uv|texcoord" n) (vector-type T-float)
      :else T-float)))

;; ── 环境与推导 ────────────────────────────────
(def empty-env {})

(declare infer-expr)

(defn infer-expr
  "推导 IR2 表达式的类型，返回 [type typed-node]。"
  [ir2-vec env]
  (let [node (first ir2-vec)
        kind (:top.kzre.homunculus.core.ir2/kind node)]
    (case kind
      :literal
      (let [t (literal-type (:val node))]
        [t (assoc node :typed-pass/type t)])

      :var
      (let [sym (:name node)
            t (get env sym T-unknown)]
        [t (assoc node :typed-pass/type t)])

      :prim
      (let [op (:op node)
            args (rest ir2-vec)
            results (map #(infer-expr % env) args)
            types (map first results)
            ret-t (infer-prim-return op types)]
        [ret-t (assoc node :typed-pass/type ret-t)])

      :call
      (let [[fn-t _] (infer-expr (second ir2-vec) env)
            args (nthrest ir2-vec 2)
            arg-results (map #(infer-expr % env) args)
            arg-types (map first arg-results)
            ret-t (if (= (:kind fn-t) :function) (:return-type fn-t) T-any)]
        [ret-t (assoc node :typed-pass/type ret-t)])

      :if
      (let [[test-t _] (infer-expr (second ir2-vec) env)
            [then-t _] (infer-expr (nth ir2-vec 2) env)
            else-ir (when (>= (count ir2-vec) 4) (nth ir2-vec 3))
            [else-t _] (if else-ir (infer-expr else-ir env) [T-void nil])
            ret-t (if (= then-t else-t) then-t T-any)]
        [ret-t (assoc node :typed-pass/type ret-t)])

      :do
      (let [exprs (map #(infer-expr % env) (rest ir2-vec))
            types (map first exprs)]
        [(last types) (assoc node :typed-pass/type (last types))])

      :let
      (let [n (:bindings-count node)
            children (rest ir2-vec)
            binds (take n children)
            body (drop n children)
            pairs (partition 2 binds)
            env' (reduce (fn [e [sym-ir val-ir]]
                           (let [sym-name (:name (first sym-ir))
                                 [val-t _] (infer-expr val-ir e)]
                             (assoc e sym-name val-t)))
                         env pairs)
            [body-t _] (infer-expr (last body) env')]
        [body-t (assoc node :typed-pass/type body-t)])

      :fn
      (let [params (or (::ir2/params node) [])
            fn-name (or (::ir2/fn-name node) nil)
            has-name (boolean fn-name)
            children (rest ir2-vec)
            param-children (if has-name (rest children) children)
            param-count (count params)
            param-irs (take param-count param-children)
            body-irs (drop param-count param-children)
            ;; 从参数子节点的 :meta 提取用户类型
            user-types (mapv (fn [p-ir]
                               (let [p-node (first p-ir)
                                     meta (:meta p-node)]
                                 (or (:user-type meta)
                                     (::user-type meta))))
                             param-irs)
            param-types (mapv (fn [sym user-t]
                                (or user-t (guess-param-type sym)))
                              params user-types)
            ;; 修正：reduce 回调解构 pair
            env2 (reduce (fn [e [sym t]]
                           (assoc e sym t))
                         env
                         (map vector params param-types))
            [ret-t _] (if (seq body-irs)
                        (infer-expr (last body-irs) env2)
                        [T-void nil])
            ftype (fn-type param-types ret-t)]
        [ftype (assoc node :typed-pass/type ftype)])

      :vector
      (let [elems (map #(infer-expr % env) (rest ir2-vec))
            types (map first elems)
            common (if (apply = types) (first types) T-any)]
        [(vector-type common) (assoc node :typed-pass/type (vector-type common))])

      :map
      (let [pairs (partition 2 (rest ir2-vec))
            key-types (map #(first (infer-expr (first %) env)) pairs)
            val-types (map #(first (infer-expr (second %) env)) pairs)
            kt (if (apply = key-types) (first key-types) T-any)
            vt (if (apply = val-types) (first val-types) T-any)]
        [(map-type kt vt) (assoc node :typed-pass/type (map-type kt vt))])

      ;; 默认：返回 void 类型
      [T-void node])))

(defn infer-program [ir2-vecs]
  (mapv (fn [ir2]
          (let [node (first ir2)
                kind (:top.kzre.homunculus.core.ir2/kind node)]
            (if (= kind :def)
              (let [val-ir (when (>= (count ir2) 3) (nth ir2 2))]
                (if val-ir
                  (let [[t new-node] (infer-expr val-ir {})]
                    (-> ir2
                        (assoc-in [2 0] new-node)
                        (assoc-in [0] (assoc node :typed-pass/type t))))
                  ir2))
              (let [[_ new-node] (infer-expr ir2 {})]
                (assoc ir2 0 new-node)))))
        ir2-vecs))