(ns top.kzre.homunculus.core.types.infer.methods.loop
  (:require [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.type :as type]))

(defmethod infer/local-infer :loop [node context]
  ;; 1. 获取所有绑定对 [var val]
  (let [bindings (n/loop-bindings node)
        ;; 2. 依次推导每个绑定，并逐步扩展类型环境（与 let 相同）
        [bind-nodes new-env]
        (reduce (fn [[bnds env] [var-node val-node]]
                  ;; 2a. 在当前环境中推导值表达式
                  (let [[val-ty val-new] (infer/local-infer val-node (infer/new-env context env))
                        var-name (n/var-name var-node)
                        ;; 2b. 若推导成功，将变量名及其类型加入环境，供后续绑定和body使用
                        env2 (if val-ty (e/extend-env env var-name val-ty) env)
                        ;; 2c. 若推导成功，将类型强制标注到变量节点上（后续 recur 可能依赖）
                        var-new (if val-ty (type/set-type! var-node val-ty) var-node)]
                    [(conj bnds [var-new val-new]) env2]))
                [[] (infer/env context)]    ;; 初始累加器
                bindings)
        ;; 3. 在新环境中推导 body（body 内部可能包含 recur，依赖当前绑定类型）
        [body-ty body-node] (infer/local-infer (n/loop-body node) (infer/new-env context new-env))]
    ;; 4. 成功条件：body 有推导出的类型
    (if body-ty
      ;; —— 成功路径 ——
      ;; 将推导后的绑定和body放回，并强制标注整个loop节点类型为body类型
      (infer/success body-ty
                     (-> node
                         (n/loop-with-children (vec bind-nodes) body-node)
                         (type/set-type! body-ty)))
      ;; —— 失败路径 ——
      ;; 仍保留子节点的推导信息，但不标注本节点类型
      (infer/nothing (n/loop-with-children node (vec bind-nodes) body-node)))))