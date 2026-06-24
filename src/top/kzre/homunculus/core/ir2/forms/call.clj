(ns top.kzre.homunculus.core.ir2.forms.call
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(def ^:private special-array-ops
  #{'%%aget '%%aset '%%new-array '%%alength})

(defmethod ir2/lower-ast :call [node env]
  (let [fn-node (first (ir2/lower-ast (n1/call-op node) env))
        args    (mapv #(first (ir2/lower-ast % env)) (n1/call-args node))]
    (if (and (n2/variable-node? fn-node)
             (contains? special-array-ops (n2/var-name fn-node)))
      (let [op (n2/var-name fn-node)]
        (case op
          %%aget
          (let [[target idx] args]
            [(n2/make-aget target idx
                           {} ;; attrs
                           (n1/node-meta node)
                           nil)])
          %%aset
          (let [[target idx val] args]
            [(n2/make-aset target idx val
                           {}
                           (n1/node-meta node)
                           nil)])
          %%new-array
          (let [[size] args]
            [(n2/make-new-array size
                                {}
                                (n1/node-meta node)
                                nil)])
          %%alength
          (let [[target] args]
            [(n2/make-alength target
                              {}
                              (n1/node-meta node)
                              nil)])))
      ;; 普通调用
      [(n2/make-call fn-node args
                     {}                          ;; attrs
                     (n1/node-meta node)
                     nil)])))