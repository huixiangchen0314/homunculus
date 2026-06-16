(ns top.kzre.homunculus.core.ir1.forms.protocol
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defn- parse-protocol-funcs [methods]
  (mapcat (fn [method-form]
            (let [method-name (first method-form)
                  arities (rest method-form)]
              (mapv (fn [arity]
                      (let [[this-sym & params] (first arity)
                            param-descs (mapv (fn [p] (n/make-param p (meta p))) params)
                            arity-meta (meta arity)]
                        (n/make-protocol-method method-name param-descs nil arity-meta)))
                    arities)))
          methods))

(defmethod ir1/form->node 'defprotocol [form]
  (let [[_ name & methods] form
        funcs (parse-protocol-funcs methods)]
    (n/make-protocol name funcs (meta form))))

(defmethod ir1/build-tree :protocol [node]
  ;; 无递归子节点，直接重建
  (n/make-protocol
    (n/protocol-name node)
    (n/protocol-funcs node)
    (n/node-meta node)))