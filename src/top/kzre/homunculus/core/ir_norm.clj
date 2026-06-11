(ns top.kzre.homunculus.core.ir-norm
  "规范化的 IR-NORM，将 IR2 转化为更加低级的 CFG 描述。
   消除 let、闭包等高级结构，仅保留基本块、显式赋值和跳转。
   适用于 HLSL 等需要结构化控制流且无法支持闭包的命令形语言。"
  (:require [top.kzre.homunculus.core.ir2 :as ir2]))

;; ═══════════════════════════════════════════════
;; 标签生成
;; ═══════════════════════════════════════════════
(def ^:private label-counter (atom 0))

(defn- fresh-label []
  (str "L" (swap! label-counter inc)))

;; ═══════════════════════════════════════════════
;; IR-Norm 节点定义
;; ═══════════════════════════════════════════════

(defn- mk [kind & kvs] (apply assoc {:kind kind} kvs))

;; 表达式
(defn literal [val type] (mk :literal :val val :type type))
(defn var-ref [name type] (mk :var :name (name name) :type type))
(defn binop [op left right type] (mk :binop :op op :left left :right right :type type))
(defn unop [op operand type] (mk :unop :op op :operand operand :type type))
(defn call-expr [func args type] (mk :call :func func :args (vec args) :type type))

;; 三元条件表达式
(defn ternary [test then else]
  (mk :ternary :test test :then then :else else
      :type (or (:type then) (:type else) {:kind :any})))

;; 语句
(defn assign [lhs rhs] (mk :assign :lhs lhs :rhs rhs))
(defn decl [name type] (mk :decl :name (name name) :type type))
(defn stmt-call [func args] (mk :stmt-call :func func :args (vec args)))

;; 基本块（自动生成唯一标签）
(defn block
  ([stmts terminator] (block nil stmts terminator))
  ([label stmts terminator]
   {:kind :block
    :label (or label (fresh-label))
    :stmts (vec stmts)
    :terminator terminator}))

;; 终止符
(defn jmp [target] (mk :jmp :target target))
(defn cjmp [test true-target false-target]
  (mk :cjmp :test test :true-target true-target :false-target false-target))
(defn ret [value] (mk :ret :value value))

;; 函数定义
(defn func [name params entry-block]
  {:kind :func, :name (name name), :params (mapv name params), :entry-block entry-block})

;; 程序
(defn program [functions]
  {:kind :program, :functions (vec functions)})

;; ═══════════════════════════════════════════════
;; 环境与辅助
;; ═══════════════════════════════════════════════

(defn- new-env []
  {:vars {}})

(defn- fresh-var [env sym]
  (let [cnt (count (:vars env))
        name (str (name sym) \_ cnt)]
    [name (update env :vars assoc sym name)]))

(defn- lookup [env sym]
  (get-in env [:vars sym] (name sym)))

;; ═══════════════════════════════════════════════
;; 表达式 lowering
;; ═══════════════════════════════════════════════
(declare lower-expr lower-stmt lower-stmts lower-fn)

(defn lower-expr [ir2-vec env]
  (let [node (first ir2-vec)
        kind (::ir2/kind node)
        t (or (:typed-pass/type node) {:kind :any})]
    (case kind
      :literal
      [(literal (:val node) t) [] env]

      :var
      (let [sym (:name node)
            vname (lookup env sym)]
        [(var-ref vname t) [] env])

      :prim
      (let [op (:op node)
            args (rest ir2-vec)
            reduced (reduce (fn [[vals stmts e] a]
                              (let [[v s e'] (lower-expr a e)]
                                [(conj vals v) (into stmts s) e']))
                            [[] [] env] args)
            arg-vals (first reduced)
            extra-stmts (second reduced)
            env' (nth reduced 2)]
        (case op
          (:add :sub :mul :div :mod :rem)
          [(binop (keyword (name op)) (arg-vals 0) (arg-vals 1) t) extra-stmts env']
          (:eq :neq :lt :lte :gt :gte)
          [(binop (keyword (name op)) (arg-vals 0) (arg-vals 1)
                  {:kind :prim, :name :bool}) extra-stmts env']
          :not
          [(unop :! (arg-vals 0) {:kind :prim, :name :bool}) extra-stmts env']
          ;; 其他原语当普通调用
          [(call-expr (name op) arg-vals t) extra-stmts env']))

      :call
      (let [[fn-val fn-stmts env1] (lower-expr (second ir2-vec) env)
            args (nthrest ir2-vec 2)
            reduced (reduce (fn [[vals stmts e] a]
                              (let [[v s e'] (lower-expr a e)]
                                [(conj vals v) (into stmts s) e']))
                            [[] fn-stmts env1] args)
            arg-vals (first reduced)
            extra-stmts (second reduced)
            env' (nth reduced 2)]
        [(call-expr fn-val arg-vals t) extra-stmts env'])

      :if
      (let [[test-val test-stmts env1] (lower-expr (second ir2-vec) env)
            [then-val then-stmts env2] (lower-expr (nth ir2-vec 2) env1)
            else-ir (when (>= (count ir2-vec) 4) (nth ir2-vec 3))
            [else-val else-stmts env3] (if else-ir
                                         (lower-expr else-ir env2)
                                         [(literal nil {:kind :void}) [] env2])]
        [(ternary test-val then-val else-val)
         (concat test-stmts then-stmts else-stmts)
         env3])

      :do
      (let [children (rest ir2-vec)
            non-last (butlast children)
            last-expr (last children)
            [non-last-vals stmts env'] (reduce (fn [[vals stmts e] a]
                                                 (let [[v s e'] (lower-expr a e)]
                                                   [(conj vals v) (into stmts s) e']))
                                               [[] [] env] non-last)
            [last-val last-stmts env''] (lower-expr last-expr env')]
        [last-val
         (concat stmts (map #(stmt-call % []) non-last-vals) last-stmts)
         env''])

      :let
      (let [bindings-count (:bindings-count node)
            all-children (rest ir2-vec)
            bind-irs (take bindings-count all-children)
            body-irs (drop bindings-count all-children)
            pairs (partition 2 bind-irs)
            [stmts env'] (reduce (fn [[stmts e] [sym-ir val-ir]]
                                   (let [sym-name (:name (first sym-ir))
                                         [val-expr val-stmts e'] (lower-expr val-ir e)
                                         [fname e''] (fresh-var e' sym-name)]
                                     [(-> stmts (into val-stmts) (conj (assign fname val-expr)))
                                      e'']))
                                 [[] env] pairs)
            [body-val body-stmts env''] (lower-expr (last body-irs) env')]
        [body-val (concat stmts body-stmts) env''])

      :fn
      (throw (ex-info "Function literals not supported in IR-Norm" {}))

      :vector
      (let [elems (rest ir2-vec)
            reduced (reduce (fn [[vals stmts e] a]
                              (let [[v s e'] (lower-expr a e)]
                                [(conj vals v) (into stmts s) e']))
                            [[] [] env] elems)]
        [(call-expr "makeArray" (first reduced) t) (second reduced) (nth reduced 2)])

      :map
      (let [pairs (partition 2 (rest ir2-vec))
            reduced (reduce (fn [[vals stmts e] [k v]]
                              (let [[kv ks e1] (lower-expr k e)
                                    [vv vs e2] (lower-expr v e1)]
                                [(conj vals kv vv) (into stmts ks vs) e2]))
                            [[] [] env] pairs)]
        [(call-expr "makeMap" (first reduced) t) (second reduced) (nth reduced 2)])

      (throw (ex-info (str "Cannot lower IR2 node to expression: " kind) {})))))

;; ═══════════════════════════════════════════════
;; 语句序列 lowering
;; ═══════════════════════════════════════════════
(defn- lower-stmts [ir2-vecs env current-block]
  (reduce (fn [[blocks e] stmt]
            (let [[new-blocks e'] (lower-stmt stmt e (last blocks))]
              [(into (vec (butlast blocks)) new-blocks) e']))
          [[current-block] env]
          ir2-vecs))

;; ═══════════════════════════════════════════════
;; 语句 lowering
;; ═══════════════════════════════════════════════
(defn lower-stmt [ir2-vec env current-block]
  (let [node (first ir2-vec)
        kind (::ir2/kind node)]
    (case kind
      :def
      (let [name-expr (second ir2-vec)
            val-ir (when (>= (count ir2-vec) 3) (nth ir2-vec 2))]
        (if (and val-ir (= (::ir2/kind (first val-ir)) :fn))
          ;; 函数定义：生成独立函数块
          (let [[func-block env'] (lower-fn val-ir env)]
            [(list current-block func-block) env'])
          ;; 全局变量
          (let [sym (:name (first name-expr))
                vname (lookup env sym)
                [rhs stmts env'] (if val-ir
                                   (lower-expr val-ir env)
                                   [(literal nil {:kind :void}) [] env])
                new-block (-> current-block
                              (update :stmts into stmts)
                              (update :stmts conj (if val-ir
                                                    (assign vname rhs)
                                                    (decl vname (:type rhs)))))]
            [[new-block] env']))))

    :let
    (let [bindings-count (:bindings-count node)
          all-children (rest ir2-vec)
          bind-irs (take bindings-count all-children)
          body-irs (drop bindings-count all-children)
          pairs (partition 2 bind-irs)
          [stmts env'] (reduce (fn [[stmts e] [sym-ir val-ir]]
                                 (let [sym-name (:name (first sym-ir))
                                       [val-expr val-stmts e'] (lower-expr val-ir e)
                                       [fname e''] (fresh-var e' sym-name)]
                                   [(-> stmts (into val-stmts) (conj (assign fname val-expr)))
                                    e'']))
                               [[] env] pairs)
          block' (reduce (fn [b s] (update b :stmts conj s)) current-block stmts)
          [blocks env''] (lower-stmts body-irs env' block')]
      [blocks env''])

    :do
    (let [children (rest ir2-vec)]
      (lower-stmts children env current-block))

    :if
    ;; 注意：当前块终止符设为 cjmp，然后创建 then/else/cont 块
    (let [then-label (fresh-label)
          else-label (fresh-label)
          cont-label (fresh-label)
          [test-val test-stmts env1] (lower-expr (second ir2-vec) env)
          then-ir (nth ir2-vec 2)
          else-ir (when (>= (count ir2-vec) 4) (nth ir2-vec 3))
          then-block (block then-label [] (jmp cont-label))
          else-block (block else-label [] (jmp cont-label))
          cont-block (block cont-label [] (ret (literal nil {:kind :void})))
          [then-blocks env2] (lower-stmt then-ir env1 then-block)
          [else-blocks env3] (if else-ir
                               (lower-stmt else-ir env2 else-block)
                               [[else-block] env2])
          term (cjmp test-val then-label else-label)
          current' (assoc current-block :terminator term)]
      [(concat [current'] then-blocks else-blocks [cont-block]) env3])

    :loop
    (let [header-label (fresh-label)
          body-label   (fresh-label)
          bindings-count (:bindings-count node)
          all-children (rest ir2-vec)
          bind-irs (take bindings-count all-children)
          body-irs (drop bindings-count all-children)
          pairs (partition 2 bind-irs)
          [stmts env'] (reduce (fn [[stmts e] [sym-ir val-ir]]
                                 (let [sym-name (:name (first sym-ir))
                                       [val-expr val-stmts e'] (lower-expr val-ir e)
                                       [fname e''] (fresh-var e' sym-name)]
                                   [(-> stmts (into val-stmts) (conj (assign fname val-expr)))
                                    e'']))
                               [[] env] pairs)
          ;; header 跳转到 body，body 最后跳回 header
          header (block header-label stmts (jmp body-label))
          body-block (block body-label [] (jmp header-label))
          [body-blocks env2] (lower-stmts body-irs env' body-block)]
      [(concat [header] body-blocks) env2])

    :recur
    (throw (ex-info "recur not yet supported in IR-Norm" {}))

    ;; 默认：表达式作为语句（丢弃值）
    (let [[val stmts env'] (lower-expr ir2-vec env)
          new-block (-> current-block
                        (update :stmts into stmts)
                        (update :stmts conj (stmt-call val [])))]
      [[new-block] env'])))

;; ═══════════════════════════════════════════════
;; 函数 lowering
;; ═══════════════════════════════════════════════
(defn- lower-fn [ir2-vec env]
  (let [node (first ir2-vec)
        params (or (::ir2/params node) [])
        name (or (::ir2/fn-name node) 'anonymous)
        body-irs (drop (count params) (rest ir2-vec))
        [param-names env'] (reduce (fn [[names e] sym]
                                     (let [[fname e'] (fresh-var e sym)]
                                       [(conj names fname) e']))
                                   [[] env] params)
        ;; 入口块初始为 ret void，随后被 body 覆盖
        entry (block [] (ret (literal nil {:kind :void})))
        [blocks env''] (lower-stmts body-irs env' entry)
        entry-block (first blocks)]
    [(func name param-names entry-block) env'']))

;; ═══════════════════════════════════════════════
;; 程序 lowering
;; ═══════════════════════════════════════════════
(defn lower [ir2-vecs]
  (let [initial-env (new-env)
        [blocks env] (lower-stmts ir2-vecs initial-env
                                  (block [] (ret (literal nil {:kind :void}))))]
    (program (filter #(= (:kind %) :func) (flatten blocks)))))