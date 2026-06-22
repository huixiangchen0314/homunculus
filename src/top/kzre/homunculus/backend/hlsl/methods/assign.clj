(ns top.kzre.homunculus.backend.hlsl.methods.assign
  "HLSL :assign 节点发射。数组整体赋值返回扁平的无标签向量。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defn- array-copy-loop-ast [target-ast val-ast size]
  (let [idx-name (gensym "i")
        idx-ref [:var-ref idx-name]]
    [[:expr-stmt [:var-decl "int" idx-name [:literal 0]]]
     [:while [:binary "<" idx-ref [:literal size]]
      ;; 循环体：两条语句均包裹 :expr-stmt
      [[:expr-stmt [:aset target-ast idx-ref [:aget val-ast idx-ref]]]
       [:expr-stmt [:binary "++" idx-ref [:literal 1]]]]]]))

(defmethod core/emit-node :assign [node context]
  (let [target-node (n/assign-var node)
        target-type (ty/get-type target-node)]
    (if (ty/vec-type? target-type)
      (array-copy-loop-ast
        (core/emit-node target-node context)
        (core/emit-node (n/assign-val node) context)
        (ty/vec-size target-type))
      [:assign (core/emit-node target-node context)
       (core/emit-node (n/assign-val node) context)])))