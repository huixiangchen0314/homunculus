(ns top.kzre.homunculus.core.ir2.pass.lambda-elim.methods.call
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.pass.lambda-elim.core :as elim]))

(defmethod elim/elim-node* :call [node env]
  (let [[new-fn fn-defs]   (elim/elim-node* (n/call-fn node) env)
        [new-args args-defs] (elim/elim-seq (n/call-args node) env)
        all-defs (into fn-defs args-defs)]
    (cond
      ;; 匿名函数直接调用
      (= :lambda (n/kind new-fn))
      (let [{:keys [new-call new-defines]} (elim/lift-and-replace-call node new-fn new-args env)]
        [new-call (into all-defs new-defines)])

      ;; 实参中包含 lambda
      :else
      (if-let [idx (first (keep-indexed (fn [i a] (when (= :lambda (n/kind a)) i)) new-args))]
        (let [lam (nth new-args idx)
              {:keys [new-call new-defines]} (elim/lift-and-replace-call node lam new-args env)]
          [new-call (into all-defs new-defines)])
        ;; 普通调用
        [(n/make-call new-fn new-args (n/attrs node) (n/node-meta node))
         all-defs]))))