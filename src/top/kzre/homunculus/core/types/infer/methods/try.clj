(ns top.kzre.homunculus.core.types.infer.methods.try
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod infer/local-infer :try [node context]
  (let [new-body    (mapv #(infer/local-infer % context) (n/try-body node))
        new-catches (mapv (fn [c]
                            ;; 递归处理 catch 内部
                            (infer/local-infer c context))
                          (n/try-catches node))
        new-finally (when-let [f (n/try-finally node)]
                      (infer/local-infer f context))]
    (infer/nothing (n/make-try (mapv second new-body)
                               (mapv second new-catches)
                               (when new-finally (second new-finally))
                               (n/attrs node) (n/node-meta node) (n/parent node)))))

(defmethod infer/local-infer :catch [node context]
  (let [new-class (infer/local-infer (n/catch-class node) context)
        new-sym   (infer/local-infer (n/catch-sym node) context)
        new-body  (mapv #(infer/local-infer % context) (n/catch-body node))]
    (infer/nothing (n/make-catch (second new-class)
                                 (second new-sym)
                                 (mapv second new-body)
                                 (n/attrs node) (n/node-meta node) (n/parent node)))))

(defmethod infer/local-infer :throw [node context]
  (let [[_ new-expr] (infer/local-infer (n/throw-expr node) context)]
    (infer/nothing (n/make-throw new-expr
                                 (n/attrs node) (n/node-meta node) (n/parent node)))))