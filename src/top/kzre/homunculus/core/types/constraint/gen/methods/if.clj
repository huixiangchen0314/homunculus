(ns top.kzre.homunculus.core.types.constraint.gen.methods.if
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.types.constraint.utils :as u]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod gen/cg-node-raw :if [node context]
  (let [[test-tv test-node test-constr test-ctx] (gen/cg-node-raw (n/if-test node) context)
        [then-tv then-node then-constr then-ctx] (gen/cg-node-raw (n/if-then node) test-ctx)
        [else-tv else-node else-constr else-ctx] (if-let [else (n/if-else node)]
                                                   (gen/cg-node-raw else then-ctx)
                                                   [nil nil nil then-ctx])
        final-ctx else-ctx
        tv (gen/fresh-tvar)
        ;; 真值约束（由前端决定）
        test-eq (when-let [req-ty (u/truthy-type-requirement context)]
                  (when test-tv
                    [(c/make-cequal test-tv (t/make-tcon req-ty))]))
        ;; 分支类型必须一致（静态语言标准行为）
        branch-eq (if then-tv
                    (if else-tv
                      [(c/make-cequal then-tv tv)
                       (c/make-cequal else-tv tv)]
                      [(c/make-cequal then-tv tv)])
                    [])
        new-node (n/make-if test-node then-node else-node
                            (n/attrs node) (n/node-meta node) (n/parent node))]
    [tv (t/set-type! new-node tv)
     (concat test-constr then-constr else-constr test-eq branch-eq)
     final-ctx]))