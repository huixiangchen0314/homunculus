(ns top.kzre.homunculus.backend.hlsl.methods.call
  "HLSL :call 节点发射。处理函数调用、运算符和采样。"
  (:require [clojure.string :as str]
            [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]))

(def ^:private infix-ops #{"+" "-" "*" "/" "%" "==" "!=" "<" ">" "<=" ">=" "&&" "||"})

(defn- remove-record-ctor-prefix-arrow [fn-name]
  (if (str/starts-with? fn-name "->")
    (subs fn-name 2)
    fn-name))

(defmethod core/emit-node :call [node context]
  (let [fn-node  (n/call-fn node)
        fn-sym   (when (= (n/kind fn-node) :variable) (n/var-name fn-node))
        fn-name  (name fn-sym)
        fn-name  (remove-record-ctor-prefix-arrow fn-name)
        args     (n/call-args node)]
    (cond
      ;; sample 特殊处理 → :sample
      (= fn-name "sample")
      [:sample (core/emit-node (first args) context)
       (core/emit-node (second args) context)
       (core/emit-node (nth args 2) context)]

      ;; 中缀运算符，且恰好 2 个实参
      (and (contains? infix-ops fn-name) (= (count args) 2))
      [:binary fn-name
       (core/emit-node (first args) context)
       (core/emit-node (second args) context)]

      ;; 普通函数调用（包括中缀但参数不是2）
      :else
      (into [:call fn-name] (mapv #(core/emit-node % context) args)))))