(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.member-access
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]))

(defmethod gen/gen-node* :member-access [current-node env]
  (let [{target-type        :type
         target-node        :node
         target-constraints :constraints
         target-env         :env}
        (gen/gen-node* (n/access-target current-node) env)
        [arg-results final-env] (gen/gen-seq (n/access-args current-node) target-env)
        arg-types        (mapv :type arg-results)
        arg-nodes        (mapv :node arg-results)
        arg-constraints  (mapcat :constraints arg-results)

        new-node         (n/make-member-access target-node
                                               (n/access-member current-node)
                                               arg-nodes
                                               (n/attrs current-node)
                                               (n/node-meta current-node))

        ret-tv           (gen/fresh-tvar)
        proj-constr      (cons/make-cproject
                           target-type
                           (n/access-member current-node)
                           ret-tv)]
    (gen/result ret-tv new-node final-env
             target-constraints arg-constraints
             proj-constr)))