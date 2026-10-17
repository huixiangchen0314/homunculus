(ns top.kzre.homunculus.backend.glsl.sematic.layout
  "语义降级规划的数据模型。只定义记录，不含逻辑。

   ── 规划内容 ──────────────────────────────────────────
   ▸ sampler 规划——HLSL 的 texture + sampler 分离声明，GLSL 合并
     为 sampler uniform。为每个 (texture, sampler) 组合分配变量名
     和纹理单元。
   ▸ 入口点规划——入口点的形参和返回值展开为全局 in/out 声明。
     为每个变量分配名字、location、flat 标记；区分内置变量与
     自定义 out。

   ── 规划是全局的 ──────────────────────────────────────
   sampler 规划跨入口点共享——同一 (texture, sampler) 组合在
   不同 shader 程序中用同一个变量名和纹理单元。
   入口点规划按入口点分组——每个入口点有自己的 in/out 列表。

   ── 内置变量 ──────────────────────────────────────────
   vertex 的 :position 和 fragment 的 :depth 对应 GLSL 内置变量
   （gl_Position / gl_FragDepth）。这类变量不声明 out——降级器
   直接写赋值。IODecl 用 builtin? 区分，var-name 是内置变量名。")

;; ═══════════════════════════════════════════
;; sampler 规划
;; ═══════════════════════════════════════════

;; 一个 sampler uniform 的规划。
(defrecord SamplerUniform
  [var-name            ;; GLSL 变量名，如 "tex0_samp0"
   texture             ;; 原始 texture 名
   sampler             ;; 原始 sampler 名
   sampler-type-str    ;; GLSL sampler 类型名字符串，如 "sampler2D" / "sampler2DShadow"
   texture-unit])      ;; 纹理单元编号，用于 glUniform1i 绑定

;; ═══════════════════════════════════════════
;; in/out 规划
;; ═══════════════════════════════════════════

;; 一个 in/out 变量的规划。
(defrecord IODecl
  [var-name        ;; GLSL 变量名。builtin? 为 true 时是内置变量名
   type            ;; IR 类型对象
   location        ;; layout(location = N)，或 nil
   flat?           ;; 是否 flat——整数插值必须 true
   builtin?])      ;; true 表示 GLSL 内置变量——降级器不声明，直接赋值

;; ═══════════════════════════════════════════
;; 接口块规划
;; ═══════════════════════════════════════════

;; 接口块成员。和 IODecl 结构类似，但缺少 builtin?——
;; 内置变量不能放进接口块。
(defrecord InterfaceBlockMember
  [var-name        ;; 成员变量名
   type            ;; IR 类型对象
   location        ;; 成员级 layout(location = N)，覆盖块级，或 nil
   flat?])         ;; 是否 flat

;; 接口块。
(defrecord InterfaceBlock
  [block-name      ;; 块名，如 "VertexData"
   instance-name   ;; 实例名，如 "vs_out" / "fs_in"
   location        ;; 块级 layout(location = N)，或 nil
   members])       ;; [InterfaceBlockMember ...]

;; ═══════════════════════════════════════════
;; 入口点规划
;; ═══════════════════════════════════════════

;; 入口点的 in/out 规划。
(defrecord EntryLayout
  [stage           ;; :vertex / :fragment
   inputs          ;; [IODecl 或 InterfaceBlock ...]
   outputs])       ;; [IODecl 或 InterfaceBlock ...]

;; ═══════════════════════════════════════════
;; 总规划
;; ═══════════════════════════════════════════

(defrecord Layout
  [samplers        ;; [SamplerUniform ...]  全局共享
   entry])         ;; EntryLayout          当前 stage 的入口点