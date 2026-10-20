# Homunculus (cljh)

Homunculus 是多编译管线架构的、面向多后端的源到源（source-to-source）编译器。旨在将 Clojure 风格的 DSL 编译到其他语言，以结合 Clojure 的范式和目标语言的生态。

## 快速开始

```clojure
(ns my.shader
  (:require [cljh.core :refer :all]))

(defuniform uViewport float2)

(defshader :fragment mainFS
           [^:user0 ^float2 pixel]
           (float4 (/ pixel.x uViewport.x)
                   (/ pixel.y uViewport.y)
                   0.0
                   1.0))
```

```bash
# 输出到 stdout（默认 HLSL）
java -jar homunculus-standalone.jar example.clj

# 指定 GLSL 后端
java -jar homunculus-standalone.jar -t glsl example.clj
```

## 编译管线

### firstorder

面向低级静态后端的编译管线。标准库为静态编译，所有函数直接内联到调用处。支持**高阶函数消除**、**闭包消除**、**类型推断**——对于不支持这些特性的后端语言，会是很强力的编程体验提升。

在这条管线中支持的后端有：

- **Shader 后端**——GLSL、HLSL、ShaderLab 等
    - 覆盖着色器语义（`defshader`、`defuniform`、语义标记、纹理 / buffer / cbuffer 声明）
    - 后端仅负责语言规则与 emit，前端与优化逻辑全部共享
    - 同一份 cljh 源码可在 GLSL 和 HLSL 之间切换，无需修改

管线流程：

```
cljh 源码
   ↓ 前端展开（宏、^:inline、内置函数）
IR2（带类型标注）
   ↓ 约束生成 → 求解（HM 推断 + 重载 + 隐式转换）
IR2（类型确定）
   ↓ 高阶函数消除（第一遍）
   ↓ 闭包消除 / lambda 提升
   ↓ 高阶函数消除（第二遍）
IRStmt（结构化：if / while / block，无 lambda）
   ↓ 优化 pass 链（迭代到不动点）
   │  ▸ role-fold       — 累积器 / 收集器 / 接收者链折叠
   │  ▸ assign-propagate — SSA 赋值传播
   │  ▸ dce              — 活跃变量分析 + 死代码消除
   ↓ ShaderAST
   ↓ 后端 emit
目标代码
```

## 标准库

标准库（`cljh.core`）提供了很多方便的函数，分为两类：

### 静态标准库

在静态编译时使用，所有函数直接内联到调用方。当不希望或无法携带运行时（RT）时使用该标准库。主要为序列函数，功能较为低级。

- `map` / `reduce` / `filter` / `conj`
- 向量 / 数组操作、值操作

无需显式 require——编译器自动注入。

### 运行时标准库（TODO）

需要依赖胶水代码和运行时，后端执行时必须携带对应的运行时。支持多方法分发、Atom、Ref 等动态特性。

## 前端

### 基础特性

- **控制流** — `if` / `let` / `loop` / `recur` / `do`
- **记录和协议** — `defrecord` / `defprotocol`，编译期消解到具体实现
- **编译指令** — `%%aget` / `%%aset` 等直接操作 IR 节点的低级原语
- **高阶函数** — 函数作为参数 / 返回值。^:inline 函数在类型约束生成阶段展开到调用点；高阶函数调用在 IR 层识别并内联。两遍消除——第一遍消源码中的高阶函数，第二遍消闭包消除产生的新高阶函数。
- **闭包** — 捕获自由变量的匿名函数。编译期 lambda 提升为独立函数，自由变量转为参数，再由第二遍高阶消除内联回调用点。源码层可见闭包，目标代码层无闭包。
- **类型推断** — 基于约束求解的 HM 风格推断。泛型高阶函数、类型变量、scheme 实例化、重载消解、隐式转换插入。可选 call-driven inference：允许调用点反向约束函数类型——静态语言下使用闭包的必要条件。

### 类型系统

基础类型构造子：

- `TCon` — 具体类型（`int` / `float4` / `sampler2D` 等）
- `TFun` — 函数类型（柯里化）
- `TVec` — 同构向量 / 定长数组（`(TVec float2 13)` 表示 `float2[13]`）
- `THeteroMap` — 异构映射（记录类型的基础）

以及编译期依赖的辅助类型：

- `TVar` — 类型变量（HM 推断的基石）
- `TValue` — 类型级值（数组长度、记录字段等编译期常量）

类型推断基于**约束求解**：每个表达式生成约束，统一求解出替换映射。支持泛型高阶函数、重载消解、隐式转换插入、以及 **call-driven inference**（允许调用点反向约束函数类型）。

用户可通过 metadata 显式标注类型，作为编译期类型提示：

```clojure
(let [^int unit (->z e0)]
  ...)
```

标注产生 coerce 约束——若值类型与标注不兼容但可转换，check 阶段插入显式转换节点。

### 宏支持

完整的 Clojure 宏语义。宏展开发生在前端阶段，展开结果进入 IR2 编译流程。

## 命令行

| 选项 | 描述 |
|------|------|
| `-h`, `--help` | 显示帮助信息 |
| `-v`, `--version` | 显示版本信息 |
| `-o`, `--output DIR` | 输出目录（默认 `out`） |
| `-t`, `--target TARGET` | 目标后端：`glsl` / `hlsl` |
| `-I`, `--include PATH` | 添加 include 搜索路径（可重复） |
| `-L`, `--lib PATH` | 添加库搜索路径（可重复） |
| `-s`, `--style STYLE` | 模块命名风格：`default` / `flat` / `flat-snake` |
| `-S`, `--split-modules` | 为每个模块生成独立的输出文件 |

## 编译

需要 [Clojure CLI 工具](https://clojure.org/guides/install_clojure) 和 JDK 8+。

```bash
make clean      # 清理
make compile    # 编译
make jar        # 打包 JAR
make uberjar    # 打包 uberjar（standalone）
```

## License

GPL3 License