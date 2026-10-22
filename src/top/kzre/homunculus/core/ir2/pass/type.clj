(ns top.kzre.homunculus.core.ir2.pass.type
  "统一的类型访问与修改工具。提供 get-type, has-type?, ensure-type, set-type! 等 API。
   确保所有 Pass 对类型的操作一致，避免覆盖错误。"
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [top.kzre.homunculus.core.ir2.ast :as ir2]
   [top.kzre.homunculus.core.ir2.pass.model :as t]
   [top.kzre.homunculus.core.ir2.pass.protocol :as p]
   [top.kzre.homunculus.core.ir2.pass.type-tokenizer :as type-tokenizer]
   [top.kzre.homunculus.internal.utils :as iu]))



;; ── 类型 kind 查询（基于 IType 协议）──

(defn type-kind
  "返回类型的 kind 关键字，若 ty 不满足 IType 协议则返回 nil。"
  [ty]
  (when (satisfies? p/IType ty)
    (p/type-kind ty)))

(defn var-type?   [ty] (= :var       (type-kind ty)))
(defn con-type?   [ty] (= :con       (type-kind ty)))
(defn fun-type?   [ty] (= :fun       (type-kind ty)))
(defn app-type?   [ty] (= :app       (type-kind ty)))
(defn scheme-type? [ty] (= :scheme        (type-kind ty)))
(defn type-value? [ty] (= :value (type-kind ty)))
(defn vec-type? [ty]
  (= :vec (type-kind ty)))
(defn hetero-vec? [ty] (= :hetero-vec (type-kind ty)))
(defn hetero-map? [ty] (= :hetero-map (type-kind ty)))

(defn type-sym
  "获取 TCon 的类型名称关键字，若非 TCon 返回 nil。"
  [ty]
  (when (con-type? ty)
    (:name ty)))

;; TODO 补充函数类型的判等逻辑
(defn type=?
  [t1 t2]
  (cond
    (and (con-type? t1) (con-type? t2))
    (= (:name t1) (:name t2))

    (and (var-type? t1) (var-type? t2))
    (= (:id t1) (:id t2))

    (and (fun-type? t1) (fun-type? t2))
    (and (type=? (:arg t1) (:arg t2))
         (type=? (:ret t1) (:ret t2)))

    (and (vec-type? t1) (vec-type? t2))
    (and (type=? (:element-type t1) (:element-type t2))
         (= (:val (:size t1)) (:val (:size t2))))

    (and (app-type? t1) (app-type? t2))
    (and (= (:ctor t1) (:ctor t2))
         (= (count (:args t1)) (count (:args t2)))
         (every? true? (map type=? (:args t1) (:args t2))))

    (and (hetero-vec? t1) (hetero-vec? t2))
    (and (= (count (:types t1)) (count (:types t2)))
         (every? true? (map type=? (:types t1) (:types t2))))

    (and (hetero-map? t1) (hetero-map? t2))
    (= (:entries t1) (:entries t2))

    (and (type-value? t1) (type-value? t2))
    (= (:val t1) (:val t2))

    :else false))

;; ── 函数类型访问器（TFun 字段，非协议）──

(defn fun-param [fn-ty]
  (:arg fn-ty))

(defn fun-ret [fn-ty]
  (:ret fn-ty))

(defn fun-params [fn-ty]
  (when (fun-type? fn-ty)
    (if (nil? (fun-param fn-ty))
      []                                    ; 零参函数
      (cons (fun-param fn-ty)
            (fun-params (fun-ret fn-ty))))))

(defn fun-return-type
  ([fn-ty]
   (if (fun-type? fn-ty)
     (recur (fun-ret fn-ty))
     fn-ty))
  ([fun-ty arity]
   ;; 消耗 arity 个参数后的剩余类型。
   ;;
   ;; 普通函数：arity=0 返回函数本身；arity=n 返回消耗 n 个参数后的类型。
   ;; 零参函数（arg 为 nil）：arity=0 返回 ret——调用零个参数即得到返回类型。
   (loop [ty fun-ty
          n  arity]
     (if (zero? n)
       (if (and (fun-type? ty) (nil? (fun-param ty)))
         (fun-ret ty)
         ty)
       (recur (fun-ret ty) (dec n))))))

(defn fun-params-return
  "解构柯里化函数，返回形参类型向量和返回值类型。

   零参函数 (TFun nil ret) 返回 {:params [] :return ret}——
   与 make-fun-type 的空参数表示、fun-return-type 的 arity=0
   语义保持一致（零参函数消耗 0 个参数即得返回类型）。

   非函数类型直接返回 {:params [] :return ty}——
   便于调用方统一处理：:return 就是\"消耗所有形参后的剩余类型\"。"
  [fn-ty]
  (loop [ty     fn-ty
         params []]
    (if (fun-type? ty)
      (if (nil? (fun-param ty))
        ;; 零参函数 (TFun nil ret)：参数链到此结束
        {:params params :return (fun-ret ty)}
        (recur (fun-ret ty) (conj params (fun-param ty))))
      {:params params :return ty})))


;; ── 容器类型访问器（TContainer 字段）──

(defn container-element-type [container-ty]
  (:element-type container-ty))

(defn container-shape [container-ty]
  (:shape container-ty))

(defn container-kind [container-ty]
  (:kind container-ty))

;; ── 类型构造器 ─────────────────────────────

(defn make-tvar [id] (t/->TVar id))
(defn make-tcon [name] (t/->TCon (iu/ensure-symbol name)))

;; 柯里化函数
(defn make-tfun [arg ret] (t/->TFun arg ret))

(defn make-tapp [ctor args] (t/->TApp ctor args))
(defn make-tvec [elem-ty size] (t/->TVec elem-ty size))
(defn ^:deprecated make-hetero-vec [types] (t/->THeteroVec types))
(defn ^:deprecated make-hetero-map [entries] (t/->THeteroMap entries))

(defn make-tvalue [val] (t/->TValue val))
;; 类型级值
(defn value-val [ty] (:val ty))

(defn- normalize-type-name
  "把类型名字面量规范化为显式 namespace 形式。

   规则：
   ▸ 已含 '/'——保留原样（namespace 已显式指定）
   ▸ 无 '/' 有 '.'——最后一个 '.' 为 namespace/name 分隔
   ▸ 无 '/' 无 '.'——原样返回

   例：
     'float4                          → 'float4
     'foo/bar                         → 'foo/bar（不变）
     'aa.bb.c                         → 'aa.bb/c
     'foo.bar                         → 'foo/bar
     'composite.tile-layout/uTileSize → 不变（已有 '/'）

   这样 'aa.bb.c 和 'aa.bb/c 解析结果一致——
   两种写法都是 TCon{name: aa.bb/c}。"
  [sym]
  (let [s (str sym)]
    (cond
      (str/includes? s "/") sym
      (str/includes? s ".")
      (let [idx (str/last-index-of s ".")]
        (symbol (subs s 0 idx) (subs s (inc idx))))
      :else sym)))

(defn- parse-type-symbol
  "解析符号字符串中的类型表达式。

   token 化由 Java 的 TypeTokenizer 完成——标识符字符集
   （含 `.` `/` `?` 和中间连字符）在 Java 侧统一处理。

   token 类型：
     :ident    —— 类型名（可能含 namespace）
     :type-var —— ?a 的 text 是 \"a\"（不含 ?）
     :int      —— 数组长度
     :arrow    —— ->
     :lbracket / :rbracket / :langle / :rangle / :lparen / :rparen / :comma
     :eof      —— 结束哨兵

   后缀优先级：[] 和 <> 高于 ->。"
  [s]
  (let [tokens (type-tokenizer/tokenize s)
        pos    (atom 0)]
    (letfn [(peek-token [] (nth tokens @pos))
            (next-token [] (let [t (peek-token)] (swap! pos inc) t))
            (peek-type  [] (:type (peek-token)))

            (parse-atom []
              (let [t (next-token)
                    tt (:type t)
                    tx (:text t)]
                (cond
                  (= tt :lparen)
                  (let [ty (parse-expr)]
                    (when (not= :rparen (peek-type))
                      (throw (ex-info "Expected )" {:s s :got (peek-token)})))
                    (next-token)
                    ty)

                  (= tt :ident)
                  (make-tcon (normalize-type-name (symbol tx)))

                  (= tt :type-var)
                  (make-tvar (symbol tx))

                  (= tt :eof)
                  (throw (ex-info "Unexpected end of type string" {:s s}))

                  :else
                  (throw (ex-info "Unexpected token in atom"
                                  {:s s :token t})))))

            (parse-generic-args []
              (let [first-arg (parse-expr)]
                (loop [args [first-arg]]
                  (let [t (next-token)]
                    (case (:type t)
                      :comma (recur (conj args (parse-expr)))
                      :rangle args
                      (throw (ex-info "Expected , or > in generic args"
                                      {:s s :token t})))))))

            (parse-postfix []
              (loop [ty (parse-atom)]
                (case (peek-type)
                  :lbracket
                  (do
                    (next-token)
                    (let [t (next-token)]
                      (when-not (= :int (:type t))
                        (throw (ex-info "Expected integer size after ["
                                        {:s s :token t})))
                      (when (not= :rbracket (peek-type))
                        (throw (ex-info "Expected ]" {:s s :token (peek-token)})))
                      (next-token)
                      (recur (t/->TVec ty (t/->TValue (parse-long (:text t)))))))

                  :langle
                  (do
                    (next-token)
                    (let [args (parse-generic-args)]
                      (recur (make-tapp ty args))))

                  ty)))

            (parse-expr []
              (let [left (parse-postfix)]
                (if (= :arrow (peek-type))
                  (do (next-token)
                      (let [right (parse-expr)]
                        (make-tfun left right)))
                  left)))]
      (let [result (parse-expr)]
        (when-not (= :eof (peek-type))
          (throw (ex-info "Trailing tokens in type string"
                          {:s s :remaining (drop @pos tokens)})))
        result))))

(defn parse-type
  [type-spec]
  (cond
    (keyword? type-spec)
    (let [s (if-let [ns-part (namespace type-spec)]
              (str ns-part "/" (name type-spec))
              (name type-spec))]
      (make-tcon (normalize-type-name (symbol s))))

    (string? type-spec)
    (parse-type-symbol type-spec)

    ;; ── S-表达式：(Buffer uint) / (Map int float) / (vec T N) ──
    (seq? type-spec)
    (let [[head & rest] type-spec]
      (cond
        ;; 数组形式：(vec T N)
        (= head 'vec)
        (let [[t n] rest]
          (when-not (= 2 (count rest))
            (throw (ex-info "vec form requires 2 args: (vec T N)"
                            {:spec type-spec})))
          (let [n* (if (integer? n) n (value-val (parse-type n)))]
            (t/->TVec (parse-type t) (t/->TValue n*))))

        ;; 泛型应用：(ctor arg1 arg2 ...)
        (symbol? head)
        (make-tapp (make-tcon (normalize-type-name head))
                   (mapv parse-type rest))

        :else
        (throw (ex-info "Invalid S-expression type form"
                        {:spec type-spec}))))

    (symbol? type-spec)
    (let [s (str type-spec)]
      (cond
        (str/starts-with? s "?")
        (make-tvar (symbol (subs s 1)))

        (or (str/includes? s "->")
            (str/includes? s "(")
            (str/includes? s "<")
            (str/includes? s "["))
        (parse-type-symbol s)

        :else
        (make-tcon (normalize-type-name type-spec))))

    :else type-spec))


(defn make-fun-type
  "从参数类型列表和返回类型构造柯里化函数类型。

   空参数列表返回 ()->ret 的 TFun 形式——
   和零参函数的类型表示一致。"
  [arg-tys ret-type]
  (if (seq arg-tys)
    (reduce (fn [ret arg] (make-tfun arg ret))
            ret-type
            (reverse arg-tys))
    (make-tfun nil ret-type)))
(defn arity->tfun
  "从符号表标准 arity 构造函数类型。"
  [arity]
  (make-fun-type (mapv :type (:params arity))
                 (some-> (:ret arity) :type)))


;; ── TVec (同构向量) 谓词与访问器 ──


(defn vec-element-type [ty]
  (when (vec-type? ty)
    (:element-type ty)))

(defn vec-size [ty]
  (when (vec-type? ty)
    (:size ty)))

;; ── THeteroVec (异构向量) 长度 ──
(defn hetero-vec-length [ty]
  (when (hetero-vec? ty)
    (count (:types ty))))

;; ── 通用向量长度（适用于同构或异构） ──
(defn vector-length-expr
  "返回向量类型的长度表达式（IR2 节点）或整数。"
  [ty]
  (cond
    (vec-type? ty)     (vec-size ty)           ; TVec 的 size 本身就是表达式节点
    (hetero-vec? ty)   (hetero-vec-length ty)  ; 整数，可包装为字面量
    :else nil))

;; ── 访问器 ─────────────────────────────────

;; TVar
(defn tvar-id [tv] (:id tv))
;; TCon 名称已有 type-sym
;; TFun 已有 fun-arg fun-ret
;; TApp
(defn tapp-ctor [ta] (:ctor ta))
(defn tapp-args [ta] (:args ta))
;; TContainer 已有 container-kind container-element-type container-shape
;; THeteroVec
(defn hetero-vec-types [hv] (:types hv))
;; THeteroMap
(defn hetero-map-entries [hm] (:entries hm))



(defn meta->type
  "从元数据 map 中提取类型标注。

   优先 :tag 键。值可以是：
   ▸ 符号   —— 简单类型（'float / 'float4）、函数类型（'int->int）、
               同构向量（'float2@13）
   ▸ 字符串 —— 同上，字符串形式

   `^float` / `^int->int` / `^float2@13` 这类 metadata 语法可以
   直接写符号——Clojure reader 对 `@` 在符号内合法。"
  ([md]
   (when md
     (when-let [type-spec (:tag md)]
       (parse-type type-spec))))
  ([md _]
   (meta->type md)))

(defn get-type
  "获取节点的类型，优先级：attrs :type > node-meta 类型标注 > nil。"
  ([node _]
   (get-type node))
  ([node]
   (or (get-in node [:attrs :type])
       (meta->type (:meta node) ))))



(defn has-type?
  "判断节点是否已有明确类型（在 attrs 或 node-meta 中）。"
  ([node known-types]
   (boolean (or (get-in node [:attrs :type])
                (meta->type node known-types))))
  ([node]
   (boolean (get-in node [:attrs :type]))))

(defn ensure-type
  "仅当节点尚未有类型时，才设置类型。返回原节点或更新后的节点。"
  ([node ty known-types]
   (if (has-type? node known-types)
     node
     (assoc-in node [:attrs :type] ty)))
  ([node ty]
   (if (has-type? node)
     node
     (assoc-in node [:attrs :type] ty))))

(defn set-type!
  "强制覆盖节点的类型。用于推断或用户明确指定的场景。"
  [node ty]
  (assoc-in node [:attrs :type] ty))



(defn concrete?
  "判断类型是否为确定的（具体）类型。
   TCon、零参函数、参数和返回都具体的函数、
   整数长度同构向量、元素全部具体的异构向量/异构 Map 均为具体。"
  [ty]
  (cond
    (con-type? ty) true
    (fun-type? ty) (and (or (nil? (fun-param ty))
                            (concrete? (fun-param ty)))
                        (concrete? (fun-ret ty)))
    (vec-type? ty) (and (concrete? (vec-element-type ty))
                        (integer? (value-val (vec-size ty))))
    (app-type? ty) (every? concrete? (tapp-args ty))
    (hetero-vec? ty) (every? concrete? (hetero-vec-types ty))
    (hetero-map? ty) (every? (fn [[_ v]] (concrete? v)) (hetero-map-entries ty))
    :else false))

(defn clear-type
  ([node]
   (clear-type node true))
  ([node force]
   (let [node' (walk/prewalk
                 (fn [x]
                   (if (and
                         (ir2/ir2? x)
                         (or force
                             (not (concrete? (get-type x)))))
                     (set-type! x nil)
                     x))
                 node)]
     node')))