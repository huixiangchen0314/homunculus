(ns top.kzre.homunculus.core.types.check.methods.loop
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :loop [node expected context]
  (let [bindings (:bindings node)
        checked-bindings (mapv (fn [[var val]]
                                 [(check/check var nil context)
                                  (check/check val nil context)])
                               bindings)
        body-node (check/check (:body node) nil context)]
    (assoc node :bindings checked-bindings :body body-node)))

(defmethod check/check :recur [node expected context]
  ;; recur 本身无类型，参数类型由 loop 绑定保证，这里仅检查参数
  (let [args (:args node)
        checked-args (mapv #(check/check % nil context) args)]
    (assoc node :args checked-args)))