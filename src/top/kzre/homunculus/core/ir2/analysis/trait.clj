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

(defn lambda-ho?
  "若 lambda 的某个参数在其函数体内被用作调用目标，则返回 true。

   严格模式：仅接受 lambda 节点。其他节点抛异常。"
  [node]
  (when (= :lambda (n/kind node))
    (let [param-names (set (map n/var-name (n/lambda-params node)))]
      (letfn [(free-var-call? [node]
                (cond
                  (n/call-node? node)
                  (let [fn-node (n/call-fn node)]
                    (or (and (n/variable-node? fn-node)
                             (contains? param-names (n/var-name fn-node)))
                        (some free-var-call? (n/children node))))
                  :else
                  (some free-var-call? (n/children node))))]
        (boolean (free-var-call? (n/lambda-body node)))))))

(defn high-order? [define-node]
  (if-let [val-node (n/define-val define-node)]
    (when (= :lambda (n/kind val-node))
      (lambda-ho? val-node))
    false))

;; ── define ──

(defn polymorphic?
  "define 绑定的 lambda 是否并非所有参数都有具体类型。
  具体类型分为两种
  1. 用户标记了完整的显示类型
  2. 推断出了具体类型"
  [define-node]
  (if-let [t (ty/get-type define-node)]
    ;; 如果有推断类型，就检查推断类型是否具体
    (not (ty/concrete? t))
    (let [val (n/define-val define-node)]
      (and (= :lambda (n/kind val))
           ;; 整体标记返回值类型，或者是推断的整体类型
           (not (ty/has-concrete-type? val))
           ;; 参数类型
           (not-any? ty/has-concrete-type? (n/lambda-params val))))))

(defn inline?
  "define 元数据是否声明 :inline true。"
  [define-node]
  (true? (:inline (n/node-meta define-node))))