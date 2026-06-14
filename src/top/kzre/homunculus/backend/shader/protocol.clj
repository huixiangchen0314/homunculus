(ns top.kzre.homunculus.backend.shader.protocol
  "着色器后端协议：抽象 HLSL/GLSL 共性的代码生成接口。
   所有方法接收的是已经生成好的字符串片段，后端只需按目标语言语法拼接。")

(defprotocol IShaderBackend
  ;; ── 类型与字面量 ──
  (shader-type           [this ir-type]
    "将 IR 类型（IType）转换为目标语言类型字符串，如 :float -> \"float\"。
     ir-type 是实现了 IType 协议的类型对象。")
  (shader-literal        [this val]
    "输出字面量文本，如 42 -> \"42\"，true -> \"true\"。")

  ;; ── 变量 ──
  (shader-var-decl       [this name ir-type mutable? init-expr]
    "生成变量声明语句。name 是变量名字符串，ir-type 是 IType，mutable? 为真时不加 const 修饰，
     init-expr 为初始化表达式字符串，若为 nil 则只声明不初始化。返回完整语句字符串（含分号）。")
  (shader-var-ref        [this name]
    "变量引用，返回变量名字符串（可能带修饰符，如 HLSL 的语义后缀）。")

  ;; ── 赋值 ──
  (shader-assign         [this var val]
    "赋值语句，var 和 val 是已生成的字符串，返回完整语句。")

  ;; ── 控制流 ──
  (shader-if             [this test then else]
    "生成 if 语句。test/then/else 均为已生成的代码字符串。
     注意：test 不应包含外层括号，后端负责添加 if(condition) 的括号。
     else 可为 nil 表示无 else 分支。")
  (shader-while          [this test body]
    "生成 while 循环。test 和 body 为已生成的字符串，后端负责添加 while(condition) 的括号。")
  (shader-block          [this stmts]
    "将多条语句（字符串序列）组合成一个代码块，自动添加大括号和必要分号/换行。")

  ;; ── 函数 ──
  (shader-function-decl  [this name params return-type body]
    "生成函数定义。name 为函数名字符串，params 为参数字符串列表（每个含类型和名字），
     return-type 为返回类型字符串，body 为函数体字符串（已格式化）。")
  (shader-return         [this expr]
    "生成 return 语句。expr 为已生成的表达式字符串。")

  ;; ── 函数调用 / 运算符 ──
  (shader-call           [this fn-name args]
    "生成函数调用或运算符表达式。fn-name 为函数名字符串，args 为参数字符串列表。
     后端可根据内置函数表将某些函数转换为中缀运算符、前缀运算符或特殊语法。
     返回完整表达式字符串（不含分号）。")

  ;; ── 类型转换 ──
  (shader-cast           [this expr src-ty dst-ty]
    "生成显式类型转换表达式。src-ty 和 dst-ty 为 IType 对象。")

  ;; ── 结构体 ──
  (shader-struct-decl    [this name members]
    "生成结构体定义。members 为向量，每个元素是 {:name :type :semantic} 的 map，
     semantic 可为 nil。")

  ;; ── 程序组合 ──
  (shader-program        [this functions structs globals entry-specs]
    "将函数定义、结构体定义、全局变量和入口包装组合成完整着色器程序。
     functions/structs/globals 是字符串列表，entry-specs 为入口描述列表，
     每个元素为 {:stage :vertex/:fragment, :fn-name \"...\", :input-params [...], :output-params [...]}。
     后端负责为每个入口生成包装，并组装出最终程序字符串。")

  ;; ── 资源声明 ──
  (shader-resource-decl  [this name res-type args]
    "生成资源声明（纹理、采样器、cbuffer 等）。
     name 为变量名字符串，res-type 为关键字（:texture2D, :sampler, :cbuffer 等），
     args 为原始 IR2 节点列表（通常是寄存器索引字面量），由后端解析。
     返回完整声明语句字符串。")

  ;; ── 结构体生成辅助 ──
  (shader-struct-from-params [this struct-name params]
    "根据参数描述生成结构体定义字符串。
     params 为 {:name :type :semantic} 的向量，semantic 可为 nil。
     返回完整的 struct 定义，如 'struct VSInput { ... };'。
     若 params 为空，可返回空字符串。")

  ;; ── 入口包装 ──
  (shader-entry-wrapper  [this stage entry-fn-name input-params output-params]
    "生成一个入口函数（如 main），在其中调用 entry-fn-name。
     stage 为 :vertex / :fragment 等。
     input-params 和 output-params 为参数描述向量，各元素 {:name :type :semantic}。
     返回完整入口函数字符串（含签名和函数体）。")

  ;; ── 全局变量声明 ──
  (shader-global-decl    [this name ir-type init-expr]
    "生成全局变量声明（如 uniform float4x4 worldMatrix;）。
     name 为变量名，ir-type 为 IType，init-expr 为初始化表达式字符串（可为 nil）。
     返回完整声明语句字符串。"))