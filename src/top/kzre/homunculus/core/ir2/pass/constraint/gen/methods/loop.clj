(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.loop
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.utils :as u]
    [top.kzre.homunculus.core.ir2.pass.env :as e]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

;; ── loop 节点约束生成 ──
(defmethod gen/gen-node* :loop [current-node context]
  (let [bindings (n/loop-bindings current-node)   ;; Binding 向量
        [bind-nodes new-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] b]
            (let [var-node (:var b)
                  val-node (:val b)
                  {:keys [type node constraints]}
                  (gen/gen-node* val-node (assoc context :env env))
                  var-name    (:name var-node)
                  binding-tv  (gen/fresh-tvar)
                  init-eq     (cons/make-cequal binding-tv type)
                  typed-var   (t/set-type! var-node binding-tv)
                  ;; 重建 binding，保留原 attrs/meta
                  new-b       (assoc b :var typed-var :val node)]
              [(conj bnds new-b)
               (e/extend-env env var-name binding-tv)
               (concat constrs constraints [init-eq])]))
          [[] (u/env context) []]
          bindings)
        loop-var-names (mapv #(:name (:var %)) bind-nodes)
        env-loop       (assoc new-env :ir2/loop-vars loop-var-names)
        {:keys [type node constraints]}
        (gen/gen-node* (n/loop-body current-node) (assoc context :env env-loop))
        new-node (n/make-loop (vec bind-nodes) node
                              (n/attrs current-node)
                              (n/node-meta current-node))]
    {:type        type
     :node        (t/set-type! new-node type)
     :constraints (concat bind-constraints constraints)
     :env         context}))

;; ── recur 节点约束生成 ──
(defmethod gen/gen-node* :recur [current-node context]
  (let [loop-var-names (get (u/env context) :ir2/loop-vars)]
    (when-not loop-var-names
      (throw (ex-info "recur outside loop" {})))
    (let [args (:args current-node)     ;; 直接访问 Recur 记录的 args 字段
          _    (when (not= (count args) (count loop-var-names))
                 (throw (ex-info "recur arg count mismatch" {})))
          results         (mapv #(gen/gen-node* % context) args)
          arg-types       (mapv :type results)
          arg-nodes       (mapv :node results)
          arg-constraints (mapcat :constraints results)
          loop-eqs        (->> (map vector arg-types loop-var-names)
                               (keep (fn [[arg-type var-name]]
                                       (when-let [variable-type
                                                  (e/lookup-env (u/env context) var-name)]
                                         (cons/make-cequal arg-type variable-type)))))
          new-node        (n/make-recur (vec arg-nodes)
                                        (n/attrs current-node)
                                        (n/node-meta current-node))]
      {:type        nil
       :node        (t/set-type! new-node nil)
       :constraints (concat arg-constraints loop-eqs)
       :env         context})))