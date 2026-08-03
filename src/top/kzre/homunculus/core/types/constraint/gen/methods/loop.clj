(ns top.kzre.homunculus.core.types.constraint.gen.methods.loop
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.types.constraint.utils :as u]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

;; ── loop 节点约束生成 ──
(defmethod gen/cg-node-raw :loop [node context]
  (let [bindings (n/loop-bindings node)         ;; Binding 向量
        [bind-nodes new-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] b]            ;; b 是 Binding 记录
            (let [var-node (:var b)
                  val-node (:val b)
                  [val-tv new-val val-constr _] (gen/cg-node-raw val-node (assoc context :env env))
                  var-name (:name var-node)
                  binding-tv (gen/fresh-tvar)
                  init-constr (c/make-cequal binding-tv val-tv)
                  typed-var (t/set-type! var-node binding-tv)
                  ;; 重建 binding，保留原 attrs/meta
                  new-b (assoc b :var typed-var :val new-val)]
              [(conj bnds new-b)
               (e/extend-env env var-name binding-tv)
               (concat constrs val-constr (list init-constr))]))
          [[] (u/env context) []]
          bindings)
        loop-var-names (mapv #(:name (:var %)) bind-nodes)
        env-loop (assoc new-env :ir2/loop-vars loop-var-names)
        [body-tv body-node body-constr _] (gen/cg-node-raw (n/loop-body node) (assoc context :env env-loop))
        new-node (n/make-loop (vec bind-nodes) body-node
                              (n/attrs node) (n/node-meta node))]
    [body-tv (t/set-type! new-node body-tv)
     (concat bind-constraints body-constr)
     context]))

;; ── recur 节点约束生成 ──
(defmethod gen/cg-node-raw :recur [node context]
  (let [loop-var-names (get (u/env context) :ir2/loop-vars)]
    (when-not loop-var-names
      (throw (ex-info "recur outside loop" {})))
    (let [args (:args node)                     ;; 直接访问 Recur 记录的 args 字段
          _ (when (not= (count args) (count loop-var-names))
              (throw (ex-info "recur arg count mismatch" {})))
          results (mapv #(gen/cg-node-raw % context) args)
          arg-tys (mapv first results)
          arg-nodes (mapv second results)
          arg-constraints (mapcat #(nth % 2) results)
          loop-eqs (->> (map vector arg-tys loop-var-names)
                        (keep (fn [[arg-ty var-name]]
                                (when-let [vty (e/lookup-env (u/env context) var-name)]
                                  (c/make-cequal arg-ty vty)))))
          new-node (n/make-recur (vec arg-nodes)
                                 (n/attrs node) (n/node-meta node))]
      [nil (t/set-type! new-node nil)
       (concat arg-constraints loop-eqs)
       context])))