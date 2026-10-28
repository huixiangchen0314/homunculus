(ns top.kzre.homunculus.core.ir2.pass.infer.methods.vector
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as env]
    [top.kzre.homunculus.core.ir2.pass.model :as t]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod infer/infer-node* :vector [node env]
  (let [[item-nodes  final-env]
       (infer/infer-seq (n/vector-items node) env)
        item-types (map :type item-nodes)
        ;; TODO 静态库编译和动态库编译要分化掉
        ;; 根据后端配置决定向量类型
        vec-type (if (env/use-hetero-vec? env)
                   ;; 支持异构向量：保留每个元素的独立类型
                   (t/->THeteroVec item-types)
                   ;; 不支持异构：构造同构向量，元素类型采用第一个元素类型
                   (let [elem-type (first item-types)]   ; 若无元素则为 nil
                     (ty/make-tvec elem-type
                                   (ty/make-tvalue (count item-types)))))
        new-node   (n/vector-with-items node item-nodes)
        typed-node (ty/set-type! new-node vec-type)]
    (if (every? some? item-types)
      (infer/success vec-type typed-node final-env)
      (infer/nothing typed-node final-env))))