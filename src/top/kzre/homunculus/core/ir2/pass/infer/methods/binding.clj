(ns top.kzre.homunculus.core.ir2.pass.infer.methods.binding
  (:require
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as env]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod infer/infer-node* :binding [node env]
  (let [var-node (:var node)
        val-node (:val node)
        [var-type var-new var-env] (infer/infer-node* var-node env)
        [val-type val-new val-env] (infer/infer-node* val-node var-env)
        ;; 标记优先于传播
        binding-type (or var-type val-type)
        var-name    (:name var-node)
        next-env    (if binding-type
                      (env/bind-var val-env var-name binding-type)
                      val-env)
        var-new     (if binding-type
                      ;; 前向传播
                      (ty/ensure-type var-new binding-type)
                      var-node)
        new-binding (assoc node :var var-new :val val-new)]
    (infer/nothing new-binding next-env)))