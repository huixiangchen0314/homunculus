(ns top.kzre.homunculus.core.types.infer.methods.member-access
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod infer/local-infer :member-access [node context]
  ;; 推断 target 部分，获得新上下文
  (let [[_ new-target ctx1] (infer/local-infer (n/access-target node) context)
        args (n/access-args node)
        ;; 顺序处理每个实参，累积新节点和上下文
        [new-args final-ctx]
        (reduce (fn [[arg-nodes ctx] arg]
                  (let [[_ new-arg new-ctx] (infer/local-infer arg ctx)]
                    [(conj arg-nodes new-arg) new-ctx]))
                [[] ctx1]
                args)
        ;; 重建成员访问节点
        new-node (n/make-member-access new-target
                                       (n/access-member node)
                                       new-args
                                       (n/node-meta node) (n/parent node))]
    (infer/nothing new-node final-ctx)))