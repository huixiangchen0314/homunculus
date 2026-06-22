(ns top.kzre.homunculus.core.types.lambda-elim.methods.call
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]
            [top.kzre.homunculus.core.types.lambda-elim.protocol :as p]))

(defn- lift-and-replace-call [call-node lam args config env]
  (let [fv        (elim/free-vars-of-lambda lam env)
        fv-vec    (vec fv)
        lifted-name (p/lift-name-gen config lam)
        new-params (if (empty? fv-vec)
                     (n/lambda-params lam)
                     (into (mapv #(n/make-variable (n/var-name %) nil nil)
                                 (n/lambda-params lam))
                           (mapv #(n/make-variable % nil nil) fv-vec)))
        new-body   (if (empty? fv-vec)
                     (n/lambda-body lam)
                     (let [bindings (mapv (fn [v] [(n/make-variable v nil nil) (n/make-variable v nil nil)]) fv-vec)]
                       (n/make-let bindings (n/lambda-body lam) {} nil nil)))
        lifted-lam (n/make-lambda new-params new-body fv-vec nil
                                  (n/attrs lam) (n/node-meta lam) nil)
        new-call   (n/make-call (n/make-variable lifted-name nil nil)
                                (if (empty? fv-vec)
                                  args
                                  (into args (mapv #(n/make-variable % nil nil) fv-vec)))
                                (n/attrs call-node) (n/node-meta call-node) nil)
        new-defines [(n/make-define lifted-name lifted-lam nil nil nil nil)]]
    {:new-call new-call
     :new-defines new-defines}))

(defmethod elim/eliminate :call [node config env]
  (let [[new-fn fn-defs] (elim/eliminate (n/call-fn node) config env)
        [new-args args-defs]
        (reduce (fn [[args defs] arg]
                  (let [[new-arg arg-defs] (elim/eliminate arg config env)]
                    [(conj args new-arg) (into defs arg-defs)]))
                [[] []]
                (n/call-args node))
        all-defs (into fn-defs args-defs)]
    (cond
      ;; 匿名函数直接调用
      (= :lambda (n/kind new-fn))
      (let [{:keys [new-call new-defines]} (lift-and-replace-call node new-fn new-args config env)]
        [new-call (into all-defs new-defines)])

      ;; 实参中包含 lambda
      :else
      (if-let [idx (first (keep-indexed (fn [i a] (when (= :lambda (n/kind a)) i)) new-args))]
        (let [lam (nth new-args idx)
              {:keys [new-call new-defines]} (lift-and-replace-call node lam new-args config env)]
          [new-call (into all-defs new-defines)])
        ;; 普通调用
        [(n/make-call new-fn new-args (n/attrs node) (n/node-meta node) (n/parent node))
         all-defs]))))