(ns top.kzre.homunculus.core.error
  "编译器错误的统一异常类型。

   - CompileException 表示「用户代码有问题，编译无法继续」
   - 其它异常（含普通 ex-info）默认视为编译器内部 bug

   ex-data 字段约定：
     :category  错误类别关键字，如 :type-mismatch / :arity-mismatch
     :node      相关 IR2 节点（如果可定位）
     :expected  期望信息（可选）
     :actual    实际信息（可选）"
  (:import
    [top.kzre.homunculus.core CompileException]))

(defn compile-error
  "构造一个 CompileException。

   category 是必填的类别关键字，用于调用方分类处理。
   data 是附加信息 map，通常包含 :node / :expected / :actual 等。"
  ([category message]
   (CompileException. message category))
  ([category message data]
   (CompileException. message category data))
  ([category message data cause]
   (CompileException. message category data cause)))

(defn compile-error?
  "判断异常是否为编译错误（用户错误）。"
  [e]
  (instance? CompileException e))

(defn category
  "获取错误的类别关键字。非 CompileException 返回 nil。"
  [e]
  (when (compile-error? e)
    (:category (ex-data e))))