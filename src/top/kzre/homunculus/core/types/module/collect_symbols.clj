(ns top.kzre.homunculus.core.types.module.collect-symbols
  "收集命名空间符号, 注册到编译上下文.
  请在 ns-pass 后.如果 强类型，请在 check-pass 后执行"
  (:require [top.kzre.homunculus.internal.protocol :as p]))


(defn collect-symbols
  "遍历 IR2 AST 收集全局符号,注册到编译上下文."
  [ir2-roots context]
  )
