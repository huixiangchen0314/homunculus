(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.try
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod gen/gen-node* :try [current-node env]
  (let [{body-type        :type
         body-node        :node
         body-constraints :constraints
         body-env         :env}
        (gen/gen-node* (n/try-body current-node) env)
        [catch-nodes catch-constraints catch-env]
        (reduce (fn [[nodes collected current-env] c]
                  (let [{:keys [node constraints env]} (gen/gen-node* c current-env)]
                    [(conj nodes node)
                     (into collected constraints)
                     env]))
                [[] [] body-env]
                (n/try-catches current-node))
        {finally-node        :node
         finally-constraints :constraints
         finally-env         :env}
        (if-let [f (n/try-finally current-node)]
          (gen/gen-node* f catch-env)
          {:node        nil
           :constraints nil
           :env         catch-env})
        tv       (or body-type (gen/fresh-tvar))
        new-node (n/make-try body-node
                             (vec catch-nodes)
                             finally-node
                             (n/attrs current-node)
                             (n/node-meta current-node))]
    {:type        tv
     :node        (ty/set-type! new-node tv)
     :constraints (concat body-constraints catch-constraints finally-constraints)
     :env         finally-env}))

(defmethod gen/gen-node* :catch [current-node env]
  (let [{class-node        :node
         class-constraints :constraints
         class-env         :env}
        (gen/gen-node* (n/catch-class current-node) env)
        {sym-node        :node
         sym-constraints :constraints
         sym-env         :env}
        (gen/gen-node* (n/catch-sym current-node) class-env)
        [body-nodes body-constraints body-env]
        (reduce (fn [[nodes collected current-env] expr]
                  (let [{:keys [node constraints env]} (gen/gen-node* expr current-env)]
                    [(conj nodes node)
                     (into collected constraints)
                     env]))
                [[] [] sym-env]
                (n/catch-body current-node))
        tv       (gen/fresh-tvar)
        new-node (n/make-catch class-node sym-node (vec body-nodes)
                               (n/attrs current-node)
                               (n/node-meta current-node))]
    {:type        tv
     :node        (ty/set-type! new-node tv)
     :constraints (concat class-constraints sym-constraints body-constraints)
     :env         body-env}))

(defmethod gen/gen-node* :throw [current-node env]
  (let [{expr-node        :node
         expr-constraints :constraints
         expr-env         :env}
        (gen/gen-node* (n/throw-expr current-node) env)
        tv       (gen/fresh-tvar)
        new-node (n/make-throw expr-node
                               (n/attrs current-node)
                               (n/node-meta current-node))]
    {:type        tv
     :node        (ty/set-type! new-node tv)
     :constraints expr-constraints
     :env         expr-env}))