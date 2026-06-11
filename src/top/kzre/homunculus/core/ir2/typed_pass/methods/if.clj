(ns top.kzre.homunculus.core.ir2.typed-pass.methods.if
  (:require [top.kzre.homunculus.core.ir2.typed-pass.core :as infer]
            [top.kzre.homunculus.core.ir2.typed-pass.types :as t]
            [top.kzre.homunculus.core.ir2.typed-pass.unify :as u]))

(defn- infer-branch [branch env]
  (if branch
    (let [[ty node] (infer/infer branch env)]
      [ty node])
    [nil nil]))

(defmethod infer/infer :if [node env]
  (let [[test-ty test-node] (infer/infer (:test node) env)
        [then-ty then-node] (infer/infer (:then node) env)
        [else-ty else-node] (infer-branch (:else node) env)]
    (u/unify test-ty (t/->TCon :bool))
    ;; 如果 then 分支是 recur，则不统一分支类型，整体类型取 else（循环退出时）
    (when (and else-ty (not= (:kind (:then node)) :recur))
      (u/unify then-ty else-ty))
    ;; 整体类型：如果有 else 且 then 不是 recur，已统一，取 then-ty；
    ;; 如果 then 是 recur，取 else-ty；否则取 then-ty（else 可能为 nil）
    (let [overall-ty (if (and else-ty (= (:kind (:then node)) :recur))
                       else-ty
                       then-ty)]
      [overall-ty
       (-> node
           (assoc :test test-node :then then-node :else else-node)
           (assoc-in [:attrs :type] overall-ty))])))