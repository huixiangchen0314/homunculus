(ns top.kzre.homunculus.core.types.inline.api
  "内联相关 Pass 的统一入口。"
  (:require
    [top.kzre.homunculus.core.types.inline.analyze :as analyze]
    [top.kzre.homunculus.core.types.inline.core :as inline-core]))

(defn analyze
  "分析 IR2 根节点，标记多态性与内联属性。"
  [ir2-roots]
  (analyze/analyze ir2-roots))

(defn process
  "根据标记执行内联，消除多态函数调用。"
  [ir2-roots context]
  (inline-core/process ir2-roots context))

(defn make-context
  "构造内联 Pass 的上下文。"
  [compile-ctx frontend backend & opts]
  (apply inline-core/make-context compile-ctx frontend backend opts))