(ns top.kzre.homunculus.backend.hlsl.methods.call
  "HLSL :call 节点发射。处理函数调用、运算符和采样。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
            [top.kzre.homunculus.core.ir2.node :as n]
            [clojure.string :as str]))

;; 中缀运算符集合
(def ^:private infix-ops #{"+" "-" "*" "/" "%" "==" "!=" "<" ">" "<=" ">=" "&&" "||"})

(defmethod core/emit-node :call [node]
  (let [fn-node  (n/call-fn node)
        fn-sym   (when (= (n/kind fn-node) :variable) (n/var-name fn-node))
        fn-name  (name fn-sym)
        args     (n/call-args node)]
    (cond
      ;; sample 特殊处理
      (= fn-name "sample")
      (let [target  (core/emit-node (first args))
            sampler (core/emit-node (second args))
            uv      (core/emit-node (nth args 2))]
        (str target ".Sample(" sampler ", " uv ")"))

      ;; 中缀运算符
      (contains? infix-ops fn-name)
      (let [arg-strs (mapv core/emit-node args)]
        (if (= (count arg-strs) 2)
          (str "(" (first arg-strs) " " fn-name " " (second arg-strs) ")")
          (str fn-name "(" (str/join ", " arg-strs) ")")))

      ;; 普通函数调用
      :else
      (str fn-name "(" (str/join ", " (mapv core/emit-node args)) ")"))))