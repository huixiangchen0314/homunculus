(ns top.kzre.homunculus.backend.shader.protocol
  "着色器后端协议：抽象 HLSL/GLSL 共性的代码生成接口。")

(defprotocol IShaderBackend
  ;; ── 类型与字面量 ──
  (shader-type           [this ir-type]
    "将 IR 类型（IType）转换为目标语言类型字符串，如 :float -> \"float\"。")
  (shader-literal        [this val]
    "输出字面量文本，如 42 -> \"42\"，true -> \"true\"。")

  ;; ── 变量 ──
  (shader-var-decl       [this name ir-type mutable? init-expr]
    "生成变量声明语句。若 init-expr 非 nil 则含初始化，如 'float x = 1.0;'；否则仅声明 'float x;'。")
  (shader-var-ref        [this name]
    "变量引用，返回变量名字符串（可能带修饰符）。")

  ;; ── 赋值 ──
  (shader-assign         [this var val]
    "赋值语句，如 'x = 5;'。")

  ;; ── 控制流 ──
  (shader-if             [this test then else]
    "if 语句，else 可为 nil 表示无 else 分支。")
  (shader-while          [this test body]
    "while 循环。")
  (shader-block          [this stmts]
    "将多条语句（字符串序列）组合成一个代码块，自动添加大括号和分号。")

  ;; ── 函数 ──
  (shader-function-decl  [this name params return-type body]
    "生成函数定义，params 为字符串列表，return-type 为字符串，body 为函数体字符串。")
  (shader-return         [this expr]
    "return 语句。")

  ;; ── 入口与阶段 ──
  (shader-entry-point    [this stage fn-name]
    "生成入口函数，调用指定函数 fn-name，stage 为 :vertex、:fragment 等，用于附加系统语义。")

  ;; ── 类型转换 ──
  (shader-cast           [this expr src-ty dst-ty]
    "显式类型转换，如 '(float)x'。")
  )