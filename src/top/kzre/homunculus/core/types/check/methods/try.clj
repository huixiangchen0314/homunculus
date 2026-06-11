(ns top.kzre.homunculus.core.types.check.methods.try
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :try [node expected context]
  (let [body-exprs (mapv #(check/check % expected context) (:body node))
        catch-exprs (mapv (fn [c] (assoc c :body (mapv #(check/check % expected context) (:body c))))
                          (:catches node))
        finally-exprs (when (:finally node)
                        (mapv #(check/check % nil context) (:finally node)))]
    (assoc node :body body-exprs :catches catch-exprs :finally finally-exprs)))

(defmethod check/check :throw [node expected context]
  ;; throw 无期望，直接返回
  node)

(defmethod check/check :catch [node expected context]
  ;; catch 一般由 try 内部调用，此处也实现以防直接调用
  (let [body-exprs (mapv #(check/check % expected context) (:body node))]
    (assoc node :body body-exprs)))