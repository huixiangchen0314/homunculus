(ns top.kzre.homunculus.core.types.typed.methods.try
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :try [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node {}]
    (let [body-irs (:body node)
          catch-irs (:catches node)
          finally-irs (:finally node)
          [body-ty body-nodes] (reduce (fn [[tys nodes] ir]
                                         (let [[ty nd] (infer/infer ir context)]
                                           [(conj tys ty) (conj nodes nd)]))
                                       [[] []]
                                       body-irs)
          main-ty (if (seq body-irs) (last body-ty) (t/->TCon :nil))
          catch-nodes (mapv (fn [c]
                              (let [cbody (:body c)
                                    [cbody-ty cbody-nodes] (reduce (fn [[tys nodes] ir]
                                                                     (let [[ty nd] (infer/infer ir context)]
                                                                       [(conj tys ty) (conj nodes nd)]))
                                                                   [[] []]
                                                                   cbody)
                                    catch-ty (if (seq cbody) (last cbody-ty) (t/->TCon :nil))]
                                (u/unify main-ty catch-ty)
                                (assoc c :body (vec cbody-nodes))))
                            catch-irs)
          finally-nodes (when (seq finally-irs)
                          (mapv #(second (infer/infer % context)) finally-irs))
          new-attrs (assoc (ir2p/attrs node) :type main-ty)]
      [main-ty (assoc node :body (vec body-nodes)
                           :catches catch-nodes
                           :finally (vec (or finally-nodes []))
                           :attrs new-attrs)])))

(defmethod infer/infer :catch [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node {}]
    (let [body-irs (:body node)
          [body-ty body-nodes] (reduce (fn [[tys nodes] ir]
                                         (let [[ty nd] (infer/infer ir context)]
                                           [(conj tys ty) (conj nodes nd)]))
                                       [[] []]
                                       body-irs)
          ty (if (seq body-irs) (last body-ty) (t/->TCon :nil))
          new-attrs (assoc (ir2p/attrs node) :type ty)]
      [ty (assoc node :body (vec body-nodes) :attrs new-attrs)])))

(defmethod infer/infer :throw [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node]
    (let [[_ expr-node] (infer/infer (:expr node) context)
          ty (t/->TCon :nil)
          new-attrs (assoc (ir2p/attrs node) :type ty)]
      [ty (assoc node :expr expr-node :attrs new-attrs)])))