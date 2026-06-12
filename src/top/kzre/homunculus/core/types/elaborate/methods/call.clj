(ns top.kzre.homunculus.core.types.elaborate.methods.call
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]
            [top.kzre.homunculus.core.types.elaborate.protocol :as cfg]
            [top.kzre.homunculus.core.types.subst :as subst]
            [top.kzre.homunculus.core.types.alpha-rename :as alpha]
            [top.kzre.homunculus.core.types.utils :as u]))

(defmethod eliminate :call [node ir2-roots config new-defs]
  (let [fn-node (:fn node)
        processed-fn (eliminate fn-node ir2-roots config new-defs)
        processed-args (mapv #(eliminate % ir2-roots config new-defs) (:args node))]
    (if (= (ir2p/kind processed-fn) :lambda)
      ;; 直接 lambda 调用
      (if (cfg/should-inline? config processed-fn node)
        (let [renamed-lam (alpha/rename processed-fn)
              inlined (subst/inline-call (assoc node :fn renamed-lam :args processed-args)
                                         renamed-lam config)]
          (eliminate inlined ir2-roots config new-defs))
        (let [fv (free-vars-of-lambda processed-fn)
              {:keys [define ref]} (subst/lift-lambda processed-fn fv config)]
          (swap! new-defs conj define)
          (eliminate (m/->CallNode ref processed-args (:attrs node) (:meta node) (:parent node))
                     ir2-roots config new-defs)))
      ;; 普通调用：检查参数中是否有 lambda 且目标函数已知
      (if-let [known-def (and (some #(= (ir2p/kind %) :lambda) processed-args)
                              (u/known-fn-name? ir2-roots processed-fn))]
        (let [target-name (:name known-def)
              lambda-idx (first (keep-indexed #(when (= (ir2p/kind %2) :lambda) %1) processed-args))]
          (if lambda-idx
            (let [{:keys [new-call new-defines]} (monomorphize (assoc node :args processed-args)
                                                               target-name lambda-idx
                                                               (nth processed-args lambda-idx)
                                                               ir2-roots config)]
              (doseq [d new-defines] (swap! new-defs conj d))
              new-call)
            (m/->CallNode processed-fn processed-args (:attrs node) (:meta node) (:parent node))))
        (m/->CallNode processed-fn processed-args (:attrs node) (:meta node) (:parent node))))))