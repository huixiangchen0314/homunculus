(ns top.kzre.homunculus.core.ir2.pass.recur-elim.env
  "recur-elim 的上下文：一次 loop 消除所需的变量名。
   对外只暴露操作——调用方不需要知道内部存了几个字段。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.utils :as u]))

(defprotocol IEnv
  (loop-var-names [this]
    "loop 绑定的变量名，按顺序返回。")
  (result-var [this]
    "保存循环结果的目标变量名。")
  (recur-flag [this]
    "驱动 while 循环的递归标志变量名。"))

(defrecord Env [var-names result-var-name recur-flag-name]
  IEnv
  (loop-var-names [_] var-names)
  (result-var     [_] result-var-name)
  (recur-flag     [_] recur-flag-name))

(defn make-env
  "为一次 loop 消除构造 env。
   result / recur-flag 的命名由 env 决定，调用方不需要知道。"
  [var-names]
  (->Env (vec var-names)
         (u/fresh-name 'result)
         (u/fresh-name 'recur?)))