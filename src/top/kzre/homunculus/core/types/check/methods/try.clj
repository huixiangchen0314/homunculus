(ns top.kzre.homunculus.core.types.check.methods.try
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check-node :try [node expected context]
  (let [body-exprs (mapv #(check/check-node % expected context) (n/try-body node))
        catch-exprs (mapv (fn [c] (assoc c :body (mapv #(check/check-node % expected context) (:body c))))
                          (n/try-catches node))
        finally-exprs (when-let [finally (n/try-finally node)]
                        (mapv #(check/check-node % nil context) finally))]
    (assoc node :body body-exprs :catches catch-exprs :finally finally-exprs)))

(defmethod check/check-node :throw [node expected context]
  node)

(defmethod check/check-node :catch [node expected context]
  (let [body-exprs (mapv #(check/check-node % expected context) (n/catch-body node))]
    (assoc node :body body-exprs)))