(ns top.kzre.homunculus.core.types.inline-lift.methods.call
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]
            [top.kzre.homunculus.core.types.free-vars :as free-vars]
            [top.kzre.homunculus.core.types.protocol :as p]
            [top.kzre.homunculus.core.types.subst :as subst]))

(defmethod walk :call [node config lifted]
  (let [fn-node (walk (:fn node) config lifted)
        args (mapv #(walk % config lifted) (:args node))]
    (if (= (ir2p/kind fn-node) :lambda)
      (if (p/should-inline? config fn-node node)
        ;; 内联：直接展开
        (subst/inline-call node fn-node config)
        ;; 提升：生成顶层 define，替换调用点
        (let [fv (free-vars/analyze fn-node)
              {:keys [define ref]} (subst/lift-lambda fn-node fv config)]
          (swap! lifted conj define)
          (m/->CallNode ref args (:attrs node) (:meta node) (:parent node))))
      (m/->CallNode fn-node args (:attrs node) (:meta node) (:parent node)))))