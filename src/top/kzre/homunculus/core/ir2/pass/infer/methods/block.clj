(ns top.kzre.homunculus.core.ir2.pass.infer.methods.block
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.type :as type]))

(defmethod infer/infer-node* :block [node env]
  (let [exprs (n/block-exprs node)
        ;; 顺序处理每个表达式，累积节点与环境，同时记录最后一个表达式的类型
        [new-exprs final-env last-type]
        (reduce (fn [[nodes current-env _] expr]
                  (let [[type new-expr new-env] (infer/infer-node* expr current-env)]
                    [(conj nodes new-expr) new-env type]))
                [[] env nil]
                exprs)
        ;; 重建 block 节点
        new-node (n/block-with-exprs node new-exprs)]
    ;; 如果最后一个表达式有类型，则 block 的整体类型为该类型
    (if last-type
      (infer/success last-type (type/set-type! new-node last-type) final-env)
      (infer/nothing new-node final-env))))