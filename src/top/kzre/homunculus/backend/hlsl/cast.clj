(ns top.kzre.homunculus.backend.hlsl.cast
  "HLSL 隐式类型转换规则。提供标量 / 向量的转换代价计算。
   代价用于重载解析排序：代价越小越优先，nil 表示不允许转换。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

;; ═══════════════════════════════════════════
;; 类型识别
;; ═══════════════════════════════════════════

(def ^:private scalar-syms
  '#{bool int uint float})

(def ^:private vector-pattern
  #"^(bool|int|uint|float)([234])$")

(defn scalar-type? [s]
  (contains? scalar-syms s))

(defn vector-type? [s]
  (boolean (re-matches vector-pattern (name s))))

(defn vec-elem [s]
  (let [[_ e _] (re-matches vector-pattern (name s))]
    (symbol e)))

(defn vec-dim [s]
  (let [[_ _ d] (re-matches vector-pattern (name s))]
    (parse-long d)))

;; ═══════════════════════════════════════════
;; 标量转换代价表
;; ═══════════════════════════════════════════

(def ^:private scalar-conv-cost
  {;; 相同类型由 (= src dst) 分支处理，不需要列
   ['bool  'int]   1
   ['bool  'uint]  1
   ['bool  'float] 1
   ['int   'uint]  2
   ['int   'float] 1
   ['uint  'int]   2
   ['uint  'float] 1
   ['float 'int]   10
   ['float 'uint]  10})

(defn scalar-cost
  "返回从 src 到 dst 的标量转换代价，nil 表示不允许。"
  [src dst]
  (cond
    (= src dst) 0
    :else       (get scalar-conv-cost [src dst])))

;; ═══════════════════════════════════════════
;; 转换代价（供 IBackendInfo/type-conversion 调用）
;; ═══════════════════════════════════════════

(defn conversion-cost
  "计算 src-ty → dst-ty 的隐式转换代价。
   返回整数代价，或 nil 表示不允许。
   support-hetero-vec? 控制向量→向量是否允许逐元素转换。"
  [src-ty dst-ty support-hetero-vec?]
  (when (and (ty/con-type? src-ty) (ty/con-type? dst-ty))
    (let [src-sym (ty/type-sym src-ty)
          dst-sym (ty/type-sym dst-ty)]
      (cond
        ;; 完全相同
        (= src-sym dst-sym) 0

        ;; 标量 → 标量
        (and (scalar-type? src-sym) (scalar-type? dst-sym))
        (scalar-cost src-sym dst-sym)

        ;; 标量 → 向量（广播）
        (and (scalar-type? src-sym) (vector-type? dst-sym))
        (when-let [c (scalar-cost src-sym (vec-elem dst-sym))]
          (inc c))                                  ; 广播代价 +1

        ;; 向量 → 向量（同维度）
        (and (vector-type? src-sym) (vector-type? dst-sym)
             (= (vec-dim src-sym) (vec-dim dst-sym))
             support-hetero-vec?)
        (scalar-cost (vec-elem src-sym) (vec-elem dst-sym))

        ;; 其余不允许
        :else nil))))