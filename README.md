# Homunculus (cljh)

Homunculus 是一个将 Clojure 方言（cljh）编译为 GPU 着色器语言（目前支持 HLSL）的编译器。它允许开发者使用类似 Clojure 的语法编写着色器，利用高阶函数、不可变数据结构和宏，同时生成高效、类型安全的 GPU 代码。

## 特性

- **Clojure 方言** – 支持 `defn`、`let`、`loop`、`if`、数组操作等常见结构，以及着色器专用语义（如 `defshader`、`defuniform`）。
- **模块化编译** – 自动解析 `ns` 依赖，递归编译，支持多文件项目。
- **类型推导** – 基于约束的类型推断，支持泛型高阶函数。
- **高阶函数内联** – 自动识别并内联多态函数，消除运行时闭包。
- **标准库** – 内置 `cljh.core`，提供 `map`、`reduce`、`filter`、`conj` 等常用函数。
- **多后端支持（计划中）** – 当前支持 HLSL，未来可扩展至 GLSL、WGSL 等。
- **LLVM 风格 CLI** – 提供简洁的命令行接口，支持单文件/多文件编译、分割输出、自定义输出目录和模块命名风格。

## 安装

### 从源码构建

需要 [Clojure CLI 工具](https://clojure.org/guides/install_clojure) 和 JDK 8+。

```bash
git clone https://github.com/huixiangchen0314/homunculus.git
cd homunculus
make uberjar   # 生成 target/homunculus-0.1.0-standalone.jar
```

### 使用预构建 JAR

下载发布的 standalone JAR，然后可直接通过 `java -jar` 运行。

## 快速开始

### 编写一个着色器文件 (`example.clj`)

```clojure
(ns my.shader
  (:require [cljh.core :refer :all]))

(defuniform worldViewProj float4x4)

(defshader :vertex vsMain
           [^:POSITION ^float4 pos]
           (mul worldViewProj pos))

(defshader :fragment psMain
           [^:SV_POSITION ^float4 pos]
           (float4 1.0 0.0 0.0 1.0))
```

### 编译

单文件输出到 stdout：

```bash
java -jar homunculus-standalone.jar example.clj
```

输出到文件（单文件）：

```bash
java -jar homunculus-standalone.jar -o out example.clj > out/result.hlsl
```

多文件分割输出（每个模块生成独立 `.hlsl` 文件）：

```bash
java -jar homunculus-standalone.jar --split-modules -o out -L src lib/a.clj lib/b.clj
```

## 命令行选项

| 选项 | 描述 |
|------|------|
| `-h`, `--help` | 显示帮助信息 |
| `-v`, `--version` | 显示版本信息 |
| `-o`, `--output DIR` | 输出目录（默认 `out`） |
| `-t`, `--target TARGET` | 目标平台（目前仅 `hlsl`） |
| `-I`, `--include PATH` | 添加 include 搜索路径（可重复） |
| `-L`, `--lib PATH` | 添加库搜索路径（可重复） |
| `-s`, `--style STYLE` | 模块命名风格：`default`, `flat`, `flat-snake` |
| `-S`, `--split-modules` | 为每个模块生成独立的输出文件 |

## 标准库

`cljh.core` 提供了常用的高阶函数，这些函数在编译时会被内联为高效的循环，不会产生运行时开销。可用函数包括：

- `map f coll`
- `reduce f init coll`
- `filter pred coll`
- `conj coll x`

无需显式 require，编译器会自动注入依赖。

## 开发与构建

```bash
# 清理
make clean

# 编译
make compile

# 打包 JAR
make jar

# 打包 uberjar
make uberjar
```

## 许可

MIT License