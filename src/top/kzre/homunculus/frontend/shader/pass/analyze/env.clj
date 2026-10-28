(ns top.kzre.homunculus.frontend.shader.pass.analyze.env
  "分析阶段的环境。承载单遍遍历的累积状态。

   环境是不可变的——所有 add-* 返回新 env。遍历器在 reduce 中携带
   env 传递。环境只累积，不提供查询——降级器消费 to-analysis 的
   输出，不直接接触 env。

   ── 职责边界 ──────────────────────────────────────────
   本环境只收集「涉及语义重分配」的信息。逐节点结构翻译的部分
   （uniform、global-var、struct、cbuffer、CS 资源与调用、函数体
   表达式、控制流）降级器直接处理 IR AST，不经过本环境。

   具体地，本环境覆盖两类降级：

   ▸ 结构降级——纹理 + 采样器合并
     IR 的 :texture-decl 和 :sampler-decl 是分离的声明，
     GLSL 需要合并为 sampler uniform。降级器必须知道哪个
     texture 被哪些 sampler 采样，才能决定生成几个 uniform。
     add-texture / add-sampler / add-sample-call 为此服务。

   ▸ 语义降级——入口点展开
     IR 的 :entry-point 的形参和返回值携带 HLSL 语义标签
     （TEXCOORD0、SV_Target 等）。GLSL 没有这些标签——参数和
     返回值要展开为全局 in/out 声明，入口函数改名为 main。
     add-entry / cs-add-entry 为此服务。

   ── 字段类型约定 ──────────────────────────────────────
   register      —— 关键字（:t0 :s1）或 nil。上游负责规范化为关键字。
   type / element-type / return-type —— IR 类型对象（TCon 等）。
                    不转关键字、不转字符串。降级器用 type-sym
                    判断类别、用 type->str 转字符串。
   param-v       —— [[name type semantic] ...] 三元组向量。
                    name 是字符串或符号，type 是 IR 类型对象，
                    semantic 是语义关键字或 nil。

   ── 前端约束 ──────────────────────────────────────────
   前端是 Clojure DSL——表达式语言，每个函数有返回值。不存在
   void 返回值。所有入口点的 return-type 永远非 nil。")

(defprotocol IEnv
  ;; ═══════════════════════════════════════════
  ;; 收集——纹理与采样器（结构降级）
  ;; ═══════════════════════════════════════════

  (add-texture [env var-name register dimension element-type]
    "记录一个纹理声明。

     降级时与配对的 sampler 合并为 GLSL 的 sampler uniform。
     dimension 与 element-type 组合决定生成哪个 GLSL sampler 类型。

     var-name     —— 变量名
     register     —— 寄存器关键字（:t0 :t1 ...）或 nil
     dimension    —— 纹理维度关键字：:1d / :2d / :3d / :cube /
                     :1d-array / :2d-array / :cube-array /
                     :2d-ms / :2d-ms-array / :buffer
     element-type —— 元素类型的 IR 类型对象（如 (TCon 'float4)）")

  (add-sampler [env var-name register compare?]
    "记录一个采样器声明。降级时与纹理配对。

     var-name  —— 变量名
     register  —— 寄存器关键字（:s0 :s1 ...）或 nil
     compare?  —— 布尔。true 时降级为 shadow sampler")

  (add-sample-call [env texture-name sampler-name]
    "记录一处采样调用，建立 texture ↔ sampler 的关系。

     同一 texture 被多个 sampler 采样时，降级器需要为每个组合
     生成独立的 sampler uniform。同一 (texture, sampler) 组合被
     多次采样时只记录一次。

     texture-name —— 纹理变量名
     sampler-name —— 采样器变量名")

  ;; ═══════════════════════════════════════════
  ;; 收集——入口点（语义降级）
  ;; ═══════════════════════════════════════════

  (add-entry [env stage entry-name return-type return-semantic param-v]
    "记录一个图形管线入口点。

     降级时 param-v 和 return-type 展开为全局 in/out 声明，入口
     函数改名为 main。参数和返回值不再是 main 的形参。

     stage           —— :vertex / :fragment
     entry-name      —— 入口函数名
     return-type     —— 返回类型的 IR 类型对象
     return-semantic —— 返回值的语义标签（如 :target0）
     param-v         —— [[name type semantic] ...] 三元组向量")

  (cs-add-entry [env entry-name param-v thread-group]
    "记录一个计算着色器入口点。

     降级时 thread-group 映射为 layout(local_size_x=..., ...)；
     param-v 是系统值（SV_DispatchThreadID 等），映射为
     gl_GlobalInvocationID 等内置变量。

     entry-name   —— 入口函数名
     param-v      —— [[name type semantic] ...] 三元组向量。
                     语义是 SV_* 系统值
     thread-group —— {:x :y :z} 的 numthreads 配置，或 nil")

  ;; ═══════════════════════════════════════════
  ;; 输出
  ;; ═══════════════════════════════════════════

  (to-analysis [env]
    "把累积状态转成 SLAnalysis 记录。降级器在遍历顶层声明前
     调用一次，后续所有降级读快照。"))