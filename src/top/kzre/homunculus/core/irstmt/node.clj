(ns top.kzre.homunculus.core.irstmt.node
  (:require [top.kzre.homunculus.core.irstmt.ast :as ast]))

(defn- block?
  [node]
  (and node (= :block (ast/kind node))))

(defn splice-block
  "把 then/else 的值规范化为 [stmts, ret-expr]。
   Block → 展开 stmts，ret 作为表达式；
   非 Block → stmts 为空，值本身作为表达式；
   nil → [[] nil]，调用方需跳过赋值。"
  [val]
  (cond
    (block? val)   [(vec (:stmts val)) (:ret val)]
    (nil? val)     [[] nil]
    :else          [[] val]))