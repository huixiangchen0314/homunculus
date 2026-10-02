(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.loop
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

;; ── loop 节点约束生成 ──
(defmethod gen/gen-node* :loop [current-node env]
  (let [bindings (n/loop-bindings current-node)   ;; Binding 向量
        [bind-nodes inner-env bind-constraints]
        (reduce
          (fn [[bnds current-env collected] b]
            (let [var-node (:var b)
                  val-node (:val b)
                  {:keys [type node constraints]}
                  (gen/gen-node* val-node current-env)
                  var-name    (:name var-node)
                  binding-tv  (gen/fresh-tvar)
                  init-eq     (cons/make-cequal binding-tv type)
                  typed-var   (t/set-type! var-node binding-tv)
                  ;; 重建 binding，保留原 attrs/meta
                  new-b       (assoc b :var typed-var :val node)]
              [(conj bnds new-b)
               (p/bind-var current-env var-name binding-tv)
               (concat collected constraints [init-eq])]))
          [[] env []]
          bindings)
        loop-var-names (mapv #(:name (:var %)) bind-nodes)
        env-loop       (p/with-loop-vars inner-env loop-var-names)
        {:keys [type node constraints]}
        (gen/gen-node* (n/loop-body current-node) env-loop)
        new-node (n/make-loop (vec bind-nodes) node
                              (n/attrs current-node)
                              (n/node-meta current-node))]
    {:type        type
     :node        (t/set-type! new-node type)
     :constraints (concat bind-constraints constraints)
     :env         env}))

;; ── recur 节点约束生成 ──
(defmethod gen/gen-node* :recur [current-node env]
  (let [loop-var-names (p/loop-vars env)]
    (when-not loop-var-names
      (throw (ex-info "recur outside loop" {})))
    (let [args (:args current-node)     ;; 直接访问 Recur 记录的 args 字段
          _    (when (not= (count args) (count loop-var-names))
                 (throw (ex-info "recur arg count mismatch" {})))
          results         (mapv #(gen/gen-node* % env) args)
          arg-types       (mapv :type results)
          arg-nodes       (mapv :node results)
          arg-constraints (mapcat :constraints results)
          loop-eqs        (->> (map vector arg-types loop-var-names)
                               (keep (fn [[arg-type var-name]]
                                       (when-let [variable-type
                                                  (p/resolve-var-type env var-name)]
                                         (cons/make-cequal arg-type variable-type)))))
          new-node        (n/make-recur (vec arg-nodes)
                                        (n/attrs current-node)
                                        (n/node-meta current-node))]
      {:type        nil
       :node        (t/set-type! new-node nil)
       :constraints (concat arg-constraints loop-eqs)
       :env         env})))