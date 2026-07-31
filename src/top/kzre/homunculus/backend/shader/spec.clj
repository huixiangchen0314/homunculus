(ns top.kzre.homunculus.backend.shader.spec
  "后端无关的着色器规范定义。包含类型、语义、入口、资源等抽象规范以及验证函数。
   所有规范均使用 clojure.spec.alpha 定义，便于后续校验与文档生成。"
  (:require [clojure.spec.alpha :as s]
            [top.kzre.homunculus.core.ir2.node :as n]))

;; ═══════════════════════════════════════════════════════════
;; 类型系统规范
;; ═══════════════════════════════════════════════════════════

;; 基本标量类型（关键字表示）
(s/def ::scalar-type #{:float :int :uint :bool})

;; 向量类型：[:vec size scalar-type]
(s/def ::vec-type (s/cat :tag #{:vec} :size pos-int? :elem-type ::scalar-type))

;; 矩阵类型：[:mat rows cols scalar-type]
(s/def ::mat-type (s/cat :tag #{:mat} :rows pos-int? :cols pos-int? :elem-type ::scalar-type))

;; 通用类型（可以是标量、向量、矩阵，或由 TCon 表示的内部类型）
;; 为兼容现有系统，暂时允许任何满足 IType 协议的值
(s/def ::type (s/or :scalar ::scalar-type
                    :vec ::vec-type
                    :mat ::mat-type
                    ;; 保持扩展性，后续可添加 IType 等
                    :itype any?))

;; ═══════════════════════════════════════════════════════════
;; 语义规范（后端无关）
;; ═══════════════════════════════════════════════════════════

;; 抽象语义关键字集合（可扩展）
(s/def ::semantic
  #{:position :normal :texcoord0 :texcoord1 :texcoord2 :texcoord3
    :texcoord4 :texcoord5 :texcoord6 :texcoord7
    :color0 :color1 :tangent :binormal :blend-indices :blend-weight
    :sv-position :sv-target :sv-depth :sv-is-front-face
    :sv-vertex-id :sv-instance-id :sv-primitive-id})

;; ═══════════════════════════════════════════════════════════
;; 着色器入口参数规范
;; ═══════════════════════════════════════════════════════════

(s/def ::param-name symbol?)
(s/def ::param (s/keys :req [::param-name ::type]
                       :opt [::semantic]))

;; ═══════════════════════════════════════════════════════════
;; 着色器入口规范
;; ═══════════════════════════════════════════════════════════

(s/def ::shader-stage #{:vertex :fragment :geometry :compute})
(s/def ::entry-name symbol?)

;; 入口完整规范：描述一个着色器入口点
(s/def ::entry-spec (s/keys :req [::stage ::entry-name ::params]
                            :opt [::return-type]))

;; ═══════════════════════════════════════════════════════════
;; 资源规范
;; ═══════════════════════════════════════════════════════════

(s/def ::resource-kind #{:texture :sampler :cbuffer})
(s/def ::bind-point (s/and int? (complement neg?)))
(s/def ::resource-name symbol?)

;; cbuffer 成员：变量名 → 类型
(s/def ::cbuffer-member-type ::type)
(s/def ::cbuffer-members (s/map-of symbol? ::cbuffer-member-type))

;; 资源通用规范
(s/def ::resource-spec (s/keys :req [::resource-name ::resource-kind ::bind-point]
                               :opt [::cbuffer-members]))

;; ═══════════════════════════════════════════════════════════
;; 模块级规范
;; ═══════════════════════════════════════════════════════════

;; 一个着色器模块（对应单个 .clj 文件）可能包含多个入口和资源
(s/def ::module-spec (s/keys :opt [::entries ::resources]))

;; ═══════════════════════════════════════════════════════════
;; 验证函数
;; ═══════════════════════════════════════════════════════════

(defn valid-entry?
  "验证 entry-spec 是否合法。"
  [entry]
  (s/valid? ::entry-spec entry))

(defn valid-resource?
  "验证 resource-spec 是否合法。"
  [resource]
  (s/valid? ::resource-spec resource))

(defn explain-entry
  "返回 entry-spec 的验证说明。"
  [entry]
  (s/explain-str ::entry-spec entry))

(defn explain-resource
  "返回 resource-spec 的验证说明。"
  [resource]
  (s/explain-str ::resource-spec resource))
