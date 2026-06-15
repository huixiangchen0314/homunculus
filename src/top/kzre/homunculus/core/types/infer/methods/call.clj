(ns top.kzre.homunculus.core.types.infer.methods.call
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defn- fun-return-type [fun-ty arity]
  (nth (iterate :ret fun-ty) arity))

(defmethod infer/local-infer :call [node context]
  ;; 1. 递归推导被调用的函数部分，得到函数类型和新节点
  (let [[fn-ty fn-node] (infer/local-infer (n/call-fn node) context)
        ;; 2. 获取所有实参，并对每个实参进行局部推导
        args (n/call-args node)
        arg-results (mapv #(infer/local-infer % context) args)  ;; 急切求值，避免重复推导
        arg-tys   (mapv first arg-results)   ;; 每个实参推导出的类型（可能为 nil）
        arg-nodes (mapv second arg-results)] ;; 每个实参推导后的新节点
    ;; 3. 判断推导是否成功：必须同时满足以下三个条件
    ;;    (a) 函数部分本身有类型（fn-ty 非 nil）
    ;;    (b) 所有实参都有推导出的类型（没有 nil，即每个实参类型已知）
    ;;    (c) 函数类型确实是函数类型（TFun），包含参数和返回类型信息
    (if (and fn-ty
             (every? some? arg-tys)   ;; some? 过滤掉 nil
             (t/fun-type? fn-ty))
      ;; —— 成功路径 ——
      (let [;; 根据函数类型和实参个数计算返回类型
            ;; fun-return-type 沿着 TFun 的 :ret 链走 arity 步，支持柯里化
            ret-ty (fun-return-type fn-ty (count arg-tys))
            ;; 重建节点：替换为推导后的子节点，并强制标注返回类型
            updated-node (-> node
                             (n/call-with-children fn-node arg-nodes)
                             (t/set-type! ret-ty))]
        (infer/success ret-ty updated-node))
      ;; —— 失败路径 ——
      ;; 即使推导失败，仍保留子节点的推导结果（fn-node, arg-nodes），
      ;; 但不标注本节点的类型（即类型为 nil）
      (infer/nothing (n/call-with-children node fn-node arg-nodes)))))