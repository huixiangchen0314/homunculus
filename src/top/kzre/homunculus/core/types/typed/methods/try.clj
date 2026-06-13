(ns top.kzre.homunculus.core.types.typed.methods.try
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :try [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
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
          new-node (type/set-type! (assoc node :body (vec body-nodes)
                                               :catches catch-nodes
                                               :finally (vec (or finally-nodes [])))
                                   main-ty)]
      [main-ty new-node {}])))

(defmethod infer/infer :catch [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [body-irs (:body node)
          [body-ty body-nodes] (reduce (fn [[tys nodes] ir]
                                         (let [[ty nd] (infer/infer ir context)]
                                           [(conj tys ty) (conj nodes nd)]))
                                       [[] []]
                                       body-irs)
          ty (if (seq body-irs) (last body-ty) (t/->TCon :nil))
          new-node (type/set-type! (assoc node :body (vec body-nodes)) ty)]
      [ty new-node {}])))

(defmethod infer/infer :throw [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [[_ expr-node] (infer/infer (:expr node) context)
          ty (t/->TCon :nil)
          new-node (type/set-type! (assoc node :expr expr-node) ty)]
      [ty new-node {}])))