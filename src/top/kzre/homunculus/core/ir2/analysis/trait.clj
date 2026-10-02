(ns top.kzre.homunculus.core.ir2.analysis.trait
  "节点固有特征查询。

   固有特征 = 只依赖节点自身、不依赖上下文 / 遍历顺序 / 任何 pass 状态。
   只读——不写 attrs，不改 IR。

   与 free-vars / var-usage 的区别：
   - trait     只看当前节点；
   - 作用域分析 看子树 + 绑定规则；
   - 使用分析   看子树 + 位置语义。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

;; ── define ──

(defn polymorphic?
  "define 绑定的 lambda 是否所有参数都无类型标注。"
  [define-node]
  (let [val (n/define-val define-node)]
    (and (= :lambda (n/kind val))
         (not-any? #(ty/has-type? %) (n/lambda-params val)))))

(defn inline?
  "define 元数据是否声明 :inline true。"
  [define-node]
  (true? (:inline (n/node-meta define-node))))