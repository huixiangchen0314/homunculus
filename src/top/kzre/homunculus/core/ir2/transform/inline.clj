(ns top.kzre.homunculus.core.ir2.transform.inline
  "内联变换：把 lambda body 的形参替换为实参，展开函数体。

   纯变换——不做策略判断：
   - 不判断是否值得内联（大小、调用次数、是否递归）
   - 不判断自由变量是否可捕获（闭包是否安全）
   - 不判断是否有非调用用法
   这些都是 pass 层的策略。

   这里只做一件事：
   给定 (call-node, lambda-node)，返回展开后的 body 节点。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.transform.replace :as replace]))

(defn inline-call
  "把 lambda-node 的形参替换为 call-node 的实参，返回展开后的 body。

   前置条件：
     1. 实参数量与形参数量一致
     2. lambda-node 无自由变量，或调用方已确认自由变量可安全代入
     3. 形参与实参无名字冲突（由 alpha 重命名保证）

   返回：展开后的 body 节点。"
  [call-node lambda-node]
  (let [params (n/lambda-params lambda-node)
        args   (n/call-args call-node)
        body   (n/lambda-body lambda-node)]
    (when-not (= (count params) (count args))
      (throw (ex-info "inline-call: arity mismatch"
                      {:params (count params)
                       :args   (count args)})))
    (reduce (fn [body [param arg]]
              (replace/replace-var body (n/var-name param) arg))
            body
            (map vector params args))))