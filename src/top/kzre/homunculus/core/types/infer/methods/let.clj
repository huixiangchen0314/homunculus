(ns top.kzre.homunculus.core.types.infer.methods.let
  (:require [top.kzre.homunculus.core.types.env :as e]
            [ top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod infer/local-infer :let [node context]
  ;; 1. 获取所有绑定对 [var val]
  (let [bindings (n/let-bindings node)
        ;; 2. 依次推导每个绑定，并逐步扩展类型环境
        [bind-nodes new-env]
        (reduce (fn [[bnds env] [var-node val-node]]
                  ;; 2a. 在当前环境中推导值表达式
                  (let [[val-ty val-new] (infer/local-infer val-node (infer/env context))
                        var-name (n/var-name var-node)
                        ;; 2b. 若推导成功，将变量名及其类型加入环境，供后续绑定和body使用
                        env2 (if val-ty (e/extend-env env var-name val-ty) env)
                        ;; 2c. 若推导成功，将类型标注到变量节点上
                        var-new (if val-ty
                                  (t/ensure-type var-node val-ty)
                                  var-node)]
                    [(conj bnds [var-new val-new]) env2]))
                [[] (:env context)]   ;; 初始累加器：空绑定列表和当前上下文环境
                bindings)
        ;; 3. 在新环境中推导 body
        [body-ty body-node] (infer/local-infer (n/let-body node) (infer/new-env context new-env))]
    ;; 4. 成功条件：body 有推导出的类型
    (if body-ty
      ;; —— 成功路径 ——
      ;; 将推导后的绑定和body放回，并强制标注整个let节点类型为body类型
      (infer/success body-ty
                     (-> node
                         (n/let-with-children (vec bind-nodes) body-node)
                         (t/set-type! body-ty)))
      ;; —— 失败路径 ——
      ;; 仍保留子节点的推导信息，但不标注本节点类型
      (infer/nothing (n/let-with-children node (vec bind-nodes) body-node)))))