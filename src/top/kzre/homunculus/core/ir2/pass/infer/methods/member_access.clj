(ns top.kzre.homunculus.core.ir2.pass.infer.methods.member-access
  (:require
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as env]
   [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
   [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defn resolve-filed-type [env target-type member]
  (when (ty/concrete? target-type)
    (let [record-name (ty/type-sym target-type)]
      (when-let [field-type (env/resolve-field-type env record-name member)]
        field-type))))

(defmethod infer/infer-node* :member-access [node env]
  ;; 推断 target 部分，获得新环境
  (let [[target-ty new-target target-env] (infer/infer-node* (n/access-target node) env)
        ;; 顺序处理每个实参，累积新节点和环境
        [new-args final-env] (infer/infer-seq (n/access-args node) target-env)
        ;; 重建成员访问节点
        new-node (n/make-member-access new-target
                                       (n/access-member node)
                                       new-args
                                       (n/attrs node)
                                       (n/node-meta node))]

    (if (n/keyword-access? new-node)
      ;; 字段访问尝试推断
      (if-let [filed-ty (resolve-filed-type env target-ty (n/access-member node))]
        (infer/success filed-ty new-node final-env)
        (infer/nothing new-node final-env))
      (infer/nothing new-node final-env))))