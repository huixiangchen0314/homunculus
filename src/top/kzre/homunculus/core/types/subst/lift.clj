(ns top.kzre.homunculus.core.types.subst.lift
  "Lambda 提升：将自由变量转化为显式参数，生成顶层定义和引用。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.protocol :as p]))

(defmulti lift-lambda
          (fn [node _free-vars _config] (n/kind node)))

(defmethod lift-lambda :lambda [lambda-node free-vars config]
  (let [lifted-name (p/lift-name-gen config lambda-node)
        new-lambda (if (seq free-vars)
                     (let [extra-params (mapv #(n/make-variable % nil nil nil) free-vars)
                           let-bindings (mapv (fn [fv]
                                                [(n/make-variable fv nil nil nil)
                                                 (n/make-variable fv nil nil nil)])
                                              free-vars)
                           new-body (n/make-let let-bindings (n/lambda-body lambda-node) {} nil nil)]
                       (n/make-lambda (into (n/lambda-params lambda-node) extra-params)
                                      new-body
                                      (n/lambda-captures lambda-node)
                                      (n/lambda-fn-name lambda-node)
                                      (n/attrs lambda-node) (n/node-meta lambda-node) nil))
                     lambda-node)
        define-node (n/make-define lifted-name new-lambda nil nil nil nil)
        ref-node    (n/make-variable (name lifted-name) nil nil nil)]
    {:define define-node :ref ref-node}))

(defmethod lift-lambda :default [node _ _]
  (throw (ex-info "Can only lift lambda nodes" {:node node})))