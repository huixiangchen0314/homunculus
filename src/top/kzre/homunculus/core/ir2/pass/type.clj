(ns top.kzre.homunculus.core.ir2.pass.type
  "统一的类型访问与修改工具。提供 get-type, has-type?, ensure-type, set-type! 等 API。
   确保所有 Pass 对类型的操作一致，避免覆盖错误。"
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [top.kzre.homunculus.core.ir2.ast :as ir2]
            [top.kzre.homunculus.core.ir2.pass.model :as t]
            [top.kzre.homunculus.core.ir2.pass.protocol :as p]
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
(defn container-type? [ty] (= :container (type-kind ty)))
(defn scheme-type? [ty] (= :scheme        (type-kind ty)))
(defn type-value? [ty] (= :value (type-kind ty)))
;; 新增谓词
(defn hetero-vec? [ty] (= :hetero-vec (type-kind ty)))
(defn hetero-map? [ty] (= :hetero-map (type-kind ty)))

(defn type-sym
  "获取 TCon 的类型名称关键字，若非 TCon 返回 nil。"
  [ty]
  (when (con-type? ty)
    (:name ty)))

;; TODO 补充函数类型的判等逻辑
(defn type=?
  "比较两个类型是否具有相同的类型名称 (type-sym)。
   当两个 type-sym 都为 nil 时，视为不相等。"
  [t1 t2]
  (let [s1 (type-sym t1)
        s2 (type-sym t2)]
    (and s1 s2 (= s1 s2))))

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
(defn make-tcon [name] (t/->TCon (iu/->symbol name)))
(defn make-tvalue [val] (t/->TValue val))
;; 柯里化函数
(defn make-tfun [arg ret] (t/->TFun arg ret))

;; 类型级值
(defn value-val [ty] (:val ty))


(defn- parse-type-symbol
  "解析符号字符串中的类型表达式。

   支持：
   ▸ 基本类型名：float / int / float4
   ▸ 函数类型：int->int->int / (int->int)->float
   ▸ 同构向量：float2@13 / int@4 / float@4@3（从内到外）

   向量后缀用 @ 分隔——`float@4@3` 表示外层 3、元素 `float[4]`。"
  [s]
  (let [tokens (re-seq #"[a-zA-Z_][a-zA-Z0-9_]*|[0-9]+|->|@|\(|\)" s)
        pos    (atom 0)]
    (letfn [(peek-token [] (nth tokens @pos nil))
            (next-token [] (let [t (peek-token)] (swap! pos inc) t))

            (parse-atom []
              (let [t (next-token)]
                (cond
                  (= t "(")
                  (let [ty (parse-expr)]
                    (when (not= (next-token) ")")
                      (throw (ex-info "Expected )" {:s s})))
                    ty)
                  (= t ")")  (throw (ex-info "Unexpected )" {:s s}))
                  (= t "->") (throw (ex-info "Unexpected ->" {:s s}))
                  (= t "@")  (throw (ex-info "Unexpected @" {:s s}))
                  (nil? t)   (throw (ex-info "Unexpected end of type string" {:s s}))
                  :else      (make-tcon (symbol t)))))

            (parse-postfix []
              (loop [ty (parse-atom)]
                (if (= (peek-token) "@")
                  (do
                    (next-token)  ; 消耗 @
                    (let [size-str (next-token)]
                      (when-not (and size-str
                                     (re-matches #"[0-9]+" size-str))
                        (throw (ex-info "Expected integer size after @"
                                        {:s s :token size-str})))
                      (recur (t/->TVec ty (t/->TValue (parse-long size-str))))))
                  ty)))

            (parse-expr []
              (let [left (parse-postfix)]
                (if (= (peek-token) "->")
                  (do (next-token)
                      (let [right (parse-expr)]
                        (make-tfun left right)))
                  left)))]
      (parse-expr))))

(defn parse-type
  "解析类型标注。

   接受：
   ▸ keyword  —— 简单类型名，如 :float
   ▸ 符号     —— 简单名、函数类型（`int->int`）、同构向量（`float2@13`）
   ▸ 字符串   —— 同上，字符串形式
   ▸ 其他     —— 视为已构造的 IR 类型对象，原样返回

   同构向量的符号形式用 @ 分隔——`float2@13` / `float@4@3`。
   不用 `[]`——Clojure reader 会把它当作独立的形式，破坏符号。"
  [type-spec]
  (cond
    (keyword? type-spec)
    (make-tcon (symbol (name type-spec)))

    (string? type-spec)
    (parse-type-symbol type-spec)

    (symbol? type-spec)
    (let [s (name type-spec)]
      (if (or (str/includes? s "->")
              (str/includes? s "(")
              (str/includes? s "@"))
        (parse-type-symbol s)
        (make-tcon type-spec)))

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

(defn make-tapp [ctor args] (t/->TApp ctor args))
(defn make-tvec [elem-ty size] (t/->TVec elem-ty size))
(defn make-hetero-vec [types] (t/->THeteroVec types))
(defn make-hetero-map [entries] (t/->THeteroMap entries))
;; 在 type.clj 中补充以下内容

;; ── TVec (同构向量) 谓词与访问器 ──
(defn vec-type? [ty]
  (= :vec (type-kind ty)))

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