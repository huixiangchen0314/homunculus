(ns top.kzre.homunculus.core.types.infer.methods.call
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))


(defmethod infer/local-infer :call [node context]
  ;; 1. 递归推导被调用的函数部分，得到函数类型、新节点和新上下文
  (let [[fn-ty fn-node fn-ctx] (infer/local-infer (n/call-fn node) context)
        ;; 2. 顺序处理所有实参，累积上下文
        args (n/call-args node)
        ;; reduce 收集：(新实参列表, 累积类型列表, 当前上下文)
        [arg-nodes arg-tys final-ctx]
        (reduce (fn [[nodes tys ctx] arg]
                  (let [[arg-ty arg-node arg-ctx] (infer/local-infer arg ctx)]
                    [(conj nodes arg-node) (conj tys arg-ty) arg-ctx]))
                [[] [] fn-ctx]
                args)]
    ;; 3. 判断推导是否成功：必须同时满足以下三个条件
    ;;    (a) 函数部分本身有类型（fn-ty 非 nil）
    ;;    (b) 所有实参都有推导出的类型（没有 nil，即每个实参类型已知）
    ;;    (c) 函数类型确实是函数类型（TFun），包含参数和返回类型信息
    (if (and fn-ty
             (every? some? arg-tys)   ;; some? 过滤掉 nil
             (t/fun-type? fn-ty))
      ;; —— 成功路径 ——
      (let [ret-ty (t/fun-return-type fn-ty (count arg-tys))
            ;; 重建节点：替换为推导后的子节点，并强制标注返回类型
            updated-node (-> node
                             (n/call-with-children fn-node arg-nodes)
                             (t/set-type! ret-ty))]
        (infer/success ret-ty updated-node final-ctx))
      ;; —— 失败路径 ——
      ;; 即使推导失败，仍保留子节点的推导结果（fn-node, arg-nodes），
      ;; 但不标注本节点的类型（即类型为 nil）
      (infer/nothing (n/call-with-children node fn-node arg-nodes) final-ctx))))