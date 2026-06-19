(ns top.kzre.homunculus.core.types.infer.methods.let
  (:require [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod infer/local-infer :let [node context]
  ;; 1. 获取所有绑定对 [var val]
  (let [bindings (n/let-bindings node)
        ;; 2. 依次推导每个绑定，逐步扩展类型环境并累积上下文
        [bind-nodes final-ctx]
        (reduce (fn [[bnds ctx] [var-node val-node]]
                  ;; 2a. 在当前上下文中推导值表达式，返回三元组
                  (let [[val-ty val-new val-ctx] (infer/local-infer val-node ctx)
                        var-name (n/var-name var-node)
                        cur-env  (infer/env val-ctx)  ; 注意：使用推导后的环境
                        ;; 2b. 若推导成功，将变量名及其类型加入环境
                        new-env (if val-ty
                                  (e/extend-env cur-env var-name val-ty)
                                  cur-env)
                        ;; 2c. 若推导成功，将类型标注到变量节点上
                        var-new (if val-ty
                                  (t/ensure-type var-node val-ty)
                                  var-node)
                        ;; 2d. 构建新的上下文，以便下一个绑定使用
                        next-ctx (if val-ty
                                   (infer/new-env val-ctx new-env)
                                   val-ctx)]  ; 即使未扩展也使用子节点返回的上下文
                    [(conj bnds [var-new val-new]) next-ctx]))
                [[] context]   ;; 初始累加器：空绑定列表和原始上下文
                bindings)
        ;; 3. 在新上下文中推导 body
        [body-ty body-node body-ctx] (infer/local-infer (n/let-body node) final-ctx)]
    ;; 4. 成功条件：body 有推导出的类型
    (if body-ty
      ;; —— 成功路径 ——
      ;; 将推导后的绑定和body放回，并强制标注整个let节点类型为body类型
      (infer/success body-ty
                     (-> node
                         (n/let-with-children (vec bind-nodes) body-node)
                         (t/set-type! body-ty))
                     body-ctx)
      ;; —— 失败路径 ——
      ;; 仍保留子节点的推导信息，但不标注本节点类型
      (infer/nothing (n/let-with-children node (vec bind-nodes) body-node) body-ctx))))