(ns top.kzre.homunculus.core.types.type
  "统一的类型访问与修改工具。提供 get-type, has-type?, ensure-type, set-type! 等 API。
   确保所有 Pass 对类型的操作一致，避免覆盖错误。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.internal.utils :as iu]
            [top.kzre.homunculus.core.types.protocol :as p]))

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

(defn fun-arg [fn-ty]
  (:arg fn-ty))

(defn fun-ret [fn-ty]
  (:ret fn-ty))

(defn fun-return-type
  ([fn-ty]
   (if (fun-type? fn-ty)
     (recur (fun-ret fn-ty))
     fn-ty))
  ([fun-ty arity]                                           ;; 知道参数个数用这个推导
   (nth (iterate :ret fun-ty) arity)))



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

;; 柯里化函数
(defn make-tfun [arg ret] (t/->TFun arg ret))

(defn arity->tfun
  "从 符号表 标准 arity 构造函数类型."
  [arity]
  (reduce (fn [ret param] (make-tfun (:type param) ret))
          (some-> (:ret arity) :type)
          (reverse (:params arity))))

(defn make-tapp [ctor args] (t/->TApp ctor args))
(defn make-tvec [element-type size] (t/->TVec element-type size))
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
(defn vector-length-expr [ty]
  "返回向量类型的长度表达式（IR2 节点）或整数。"
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
;; FixedLength
(defn fixed-length-size [fl] (:size fl))
;; VariableLength 无字段



(defn meta->type
  "从元数据 map 中提取类型标注。
   优先 :tag 符号（如 ^float），其次遍历关键字键，
   与 known-types（符号集合）匹配后返回 TCon。"
  [md known-types]
  (when md
    (let [type-sym (:tag md)]
      (when (and type-sym (contains? (set known-types) type-sym))
        (t/->TCon type-sym)))))

(defn get-type
  "获取节点的类型，优先级：attrs :type > node-meta 类型标注 > nil。"
  ([node known-types]
   (or (get-in node [:attrs :type])
       (meta->type (ir2p/node-meta node) known-types)))
  ([node]
   (get-in node [:attrs :type])))



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
   TCon、整数长度同构向量、元素全部具体的异构向量/异构 Map 均为具体。"
  [ty]
  (cond
    (con-type? ty) true
    (fun-type? ty) (and (concrete? (fun-arg ty))
                        (concrete? (fun-ret ty)))
    (vec-type? ty) (and (concrete? (vec-element-type ty))
                        (let [sz (vec-size ty)]
                          (and sz (not (var-type? sz)))))
    (hetero-vec? ty) (every? concrete? (hetero-vec-types ty))
    (hetero-map? ty) (every? (fn [[_ v]] (concrete? v)) (hetero-map-entries ty))
    :else false))
