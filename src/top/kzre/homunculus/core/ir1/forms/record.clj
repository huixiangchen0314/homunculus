(ns top.kzre.homunculus.core.ir1.forms.record
  (:require
    [top.kzre.homunculus.core.ir1.core :as ir1]
    [top.kzre.homunculus.core.ir1.ast :as m]
    [top.kzre.homunculus.core.ir1.node :as n]))

(defn- parse-fields [field-vec]
  (->> field-vec
       (filter symbol?)
       (mapv (fn [sym] (m/->Field sym (meta sym))))))

(defn- parse-method [method-form env]
  (let [[method-name params-vec & body-exprs] method-form
        docstring   (when (string? (first body-exprs)) (first body-exprs))
        body-forms  (if docstring (rest body-exprs) body-exprs)
        method-meta (merge (meta method-name) (meta method-form) (meta params-vec))
        params      (rest params-vec)
        env-params  (reduce (fn [e p] (ir1/bind-local-symbol e p)) env params)
        param-nodes (mapv (fn [p] (m/->Param p (meta p))) params)
        [body-nodes _] (ir1/lower-seq body-forms env-params)
        ir-body     (n/wrap-body body-nodes (meta body-forms))]
    (m/->Method method-name param-nodes ir-body docstring method-meta)))

(defn- parse-protocols [body-forms env]
  (loop [forms body-forms
         current-proto nil
         result []]
    (if-let [form (first forms)]
      (if (symbol? form)
        (recur (rest forms)
               {:proto-name form :methods []}
               (if current-proto (conj result current-proto) result))
        (let [method (parse-method form env)]
          (if current-proto
            (recur (rest forms)
                   (update current-proto :methods conj method)
                   result)
            (throw (ex-info "Method without protocol" {:method form})))))
      (if current-proto (conj result current-proto) result))))

(defmethod ir1/lower-node* 'defrecord [form env]
  (let [[_ name field-vec & body-forms] form
        fields         (parse-fields field-vec)
        proto-maps     (parse-protocols body-forms env)
        protocol-impls (mapv (fn [pm]
                               (m/->ProtocolImpl (:proto-name pm)
                                                 (:methods pm)
                                                 (meta form)))
                             proto-maps)]
    [(m/->Record name fields protocol-impls (meta form)) env]))