(ns top.kzre.homunculus.backend.hlsl.cast
  "HLSL 隐式类型转换规则。提供标量 / 向量的转换代价计算。
   代价用于重载解析排序：代价越小越优先，nil 表示不允许转换。

   ── 代价设计原则 ────────────────────────────────────

   1. 完全匹配 → 0
   2. 同类内轻微转换 → 1
      · bool → 数值（真值转换，语义明确）
      · int → uint（非负整数范围重叠，C 标准行为）
   3. 同类内较大转换 → 2
      · uint → int（可能溢出，C 标准行为是警告）
      · int / uint → float（精度损失，>= 2^24 时）
   4. 跨类高风险转换 → 10
      · float → int / uint（截断 + 溢出）

   ── 为什么 int → uint 比 uint → int 便宜 ─────────────

   整数算术表达式中的字面量和小变量绝大多数是非负的——int → uint
   在这一场景下是\"安全的拓宽\"。而 uint → int 对 > INT_MAX 的值是
   未定义行为，风险更高。这与 C 的 usual arithmetic conversions
   一致：当 int 和 unsigned int 混合运算时，int 转换为 unsigned int。

   这个不对称让 `(* tidx 2)`（tidx : uint, 2 : int）优先选
   `uint * uint -> uint`——保持 uint 语义，而不是隐式转成 int 或 float。

   ── 与字面量的关系 ──────────────────────────────────

   字面量的类型只取决于字面量本身——2 是 int，2.0 是 float。
   字面量不根据上下文漂移类型。转换代价表对\"字面量 int\"和
   \"变量 int\"使用相同的规则——代价差异完全由类型对决定。"
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
;;
;; 对称性：(src, dst) 与 (dst, src) 的代价**不**要求相等——
;; 转换的方向性反映语义风险的不对称。

(def ^:private scalar-conv-cost
  {;; ── bool → 数值：真值转换，语义明确，代价 1 ──
   ['bool  'int]   1
   ['bool  'uint]  1
   ['bool  'float] 1

   ;; ── 整数之间：int → uint 便宜，uint → int 贵 ──
   ;;    与 C 的 usual arithmetic conversions 一致：
   ;;    int + unsigned → unsigned（int 拓宽到 unsigned）
   ['int   'uint]  1
   ['uint  'int]   2

   ;; ── 整数 → 浮点：精度损失（> 2^24 时），代价 2 ──
   ['int   'float] 2
   ['uint  'float] 2

   ;; ── 浮点 → 整数：截断 + 溢出风险，代价 10 ──
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