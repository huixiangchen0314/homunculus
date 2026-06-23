(ns top.kzre.homunculus.core.types.inline.analyze
  "分析 Pass：检测函数定义的多态性和内联标记。
   - 若函数所有参数均无类型标注，则标记为 :polymorphic true。
   - 若函数元数据包含 :inline true，则标记为 :inline true。
   结果存入 define 节点的 attrs 中。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]))

(defn- analyze-define [node]
  (let [val   (n/define-val node)
        meta  (n/node-meta node)]
    (when (and val (= :lambda (n/kind val)))
      (let [params      (n/lambda-params val)
            ;; 检查参数是否有类型标注（通过 attrs 或已有 type 属性）
            any-typed?  (some #(ty/has-type? %) params)
            polymorphic? (not any-typed?)
            ;; 从 define 的元数据中读取 :inline 标记
            inline?     (true? (:inline meta))]
        ;; 将分析结果更新到 define 节点的 attrs 中
        (-> node
            (assoc-in [:attrs :polymorphic] polymorphic?)
            (assoc-in [:attrs :inline] inline?))))))

(defn analyze
  "遍历 IR2 根节点，对每个顶层 define 进行分析，更新 attrs。"
  [ir2-roots]
  (mapv (fn [root]
          (if (= (n/kind root) :define)
            (or (analyze-define root) root)
            root))
        ir2-roots))