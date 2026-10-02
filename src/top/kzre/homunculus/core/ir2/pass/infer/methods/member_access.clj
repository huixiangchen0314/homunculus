(ns top.kzre.homunculus.core.ir2.pass.infer.methods.member-access
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]))

(defmethod infer/infer-node* :member-access [node env]
  ;; 推断 target 部分，获得新环境
  (let [[_ new-target target-env] (infer/infer-node* (n/access-target node) env)
        ;; 顺序处理每个实参，累积新节点和环境
        [new-args final-env]
        (reduce (fn [[arg-nodes current-env] arg]
                  (let [[_ new-arg new-env] (infer/infer-node* arg current-env)]
                    [(conj arg-nodes new-arg) new-env]))
                [[] target-env]
                (n/access-args node))
        ;; 重建成员访问节点
        new-node (n/make-member-access new-target
                                       (n/access-member node)
                                       new-args
                                       (n/attrs node)
                                       (n/node-meta node))]
    (infer/nothing new-node final-env)))