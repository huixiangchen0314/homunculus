(ns top.kzre.homunculus.backend.hlsl.methods.define
  "HLSL :define 节点发射。所有子结构由分派处理，define 仅做顶层包装。"
  (:require
    [top.kzre.homunculus.backend.hlsl.core :as core]
    [top.kzre.homunculus.backend.util.naming :refer [cname]]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]))

(defn- array-copy-loop-ast [target-ast val-ast size]
  (let [idx-name (gensym "i")
        idx-ref [:var-ref idx-name]]
    [[:expr-stmt [:var-decl "int" idx-name [:literal 0]]]
     [:while [:binary "<" idx-ref [:literal size]]
      [[:expr-stmt [:aset target-ast idx-ref [:aget val-ast idx-ref]]]
       [:expr-stmt [:binary "++" idx-ref [:literal 1]]]]]]))

(defmethod core/emit-node :define [node context]
  (let [type (ty/get-type node)]
    (if (ty/fun-type? type)
      ;; 函数定义
      (let [lam       (n/define-val node)
            fn-ty     (ty/get-type lam)
            ret-type  (core/hlsl-type-str (ty/fun-return-type fn-ty))
            params    (n/lambda-params lam)
            param-vecs (mapv (fn [p]
                               [(core/hlsl-type-str (ty/get-type p))
                                (cname (n/var-name p))])
                             params)
            raw-body  (core/emit-node (n/lambda-body lam) context)
            body-vec  (if (and (vector? raw-body) (not (keyword? (first raw-body))))
                        raw-body
                        [raw-body])
            last-elem (last body-vec)
            ;; 确保最后是 return 语句，并包裹 :expr-stmt
            body'     (if (and (vector? last-elem) (= :return (first last-elem)))
                        body-vec   ; 假设已被正确包裹（block/let 会保证）
                        (conj (vec (butlast body-vec))
                              [:expr-stmt [:return last-elem]]))]
        [:function ret-type (cname (n/define-name node)) param-vecs body'])

      ;; 变量/数组定义
      (let [val (n/define-val node)]
        (if (= :new-array (n/kind val))
          ;; 数组声明作为语句
          [:expr-stmt
           [:array-decl
            (core/hlsl-type-str (ty/vec-element-type type))
            (cname (n/define-name node))
            (core/emit-node (n/new-array-size val) context)]]
          (if (ty/vec-type? type)
            ;; 数组类型（非 new-array）：从右值分离前置语句和最终表达式
            (let [val-ast   (core/emit-node val context)
                  [pre-stmts val-expr] (if (and (vector? val-ast) (not (keyword? (first val-ast))))
                                         [(butlast val-ast) (last val-ast)]
                                         [[] val-ast])
                  size      (ty/vec-size type)
                  decl      [:expr-stmt
                             [:array-decl
                              (core/hlsl-type-str (ty/vec-element-type type))
                              (cname (n/define-name node))
                              [:literal size]]]
                  target-ref [:var-ref (cname (n/define-name node))]
                  copy-loop (array-copy-loop-ast target-ref val-expr size)]
              ;; 前置语句 + 声明 + 拷贝循环
              (vec (concat pre-stmts [decl] copy-loop)))
            ;; 标量/向量声明作为语句
            [:expr-stmt
             [:var-decl
              (core/hlsl-type-str (ty/get-type val))
              (cname (n/define-name node))
              (core/emit-node val context)]]))))))