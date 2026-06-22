(ns top.kzre.homunculus.core.types.constraint.gen.methods.loop
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.types.constraint.utils :as u]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

;; ── loop 节点约束生成 ──
(defmethod gen/cg-node-raw :loop [node context]
  (let [bindings (n/loop-bindings node)
        ;; 为每个绑定创建全新的类型变量，避免过早实例化
        [bind-nodes new-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] [var val]]
            (let [[val-tv val-node val-constr _val-ctx] (gen/cg-node-raw val (assoc context :env env))
                  var-name (n/var-name var)
                  ;; 创建独立的 binding-tv，与初始值类型解耦
                  binding-tv (gen/fresh-tvar)
                  init-constr (c/make-cequal binding-tv val-tv)   ;; 初始值类型与 binding-tv 统一
                  var-node (t/set-type! var binding-tv)]
              [(conj bnds [var-node val-node])
               (e/extend-env env var-name binding-tv)
               (concat constrs val-constr (list init-constr))]))
          [[] (u/env context) []]
          bindings)
        ;; 记录 loop 变量名，供 recur 使用
        loop-var-names (mapv (fn [[v _]] (n/var-name v)) bind-nodes)
        env-loop (assoc new-env :ir2/loop-vars loop-var-names)
        ;; 在包含 loop 变量的环境中推导 body
        [body-tv body-node body-constr _body-ctx] (gen/cg-node-raw (n/loop-body node) (assoc context :env env-loop))
        new-node (n/make-loop (vec bind-nodes) body-node
                              (n/attrs node) (n/node-meta node) (n/parent node))]
    [body-tv (t/set-type! new-node body-tv)
     (concat bind-constraints body-constr)
     context]))  ;; 内部绑定不泄露

;; ── recur 节点约束生成 ──
(defmethod gen/cg-node-raw :recur [node context]
  (let [loop-var-names (get (u/env context) :ir2/loop-vars)]
    (when-not loop-var-names
      (throw (ex-info "recur outside loop" {})))
    (let [args (n/recur-args node)
          _ (when (not= (count args) (count loop-var-names))
              (throw (ex-info "recur arg count mismatch" {})))
          results (mapv #(gen/cg-node-raw % context) args)   ;; 急切求值，解构四元组
          arg-tys (mapv first results)
          arg-nodes (mapv second results)
          arg-constraints (mapcat #(nth % 2) results)
          ;; 生成 recur 实参与对应 loop 变量的相等约束
          loop-eqs (->> (map vector arg-tys loop-var-names)
                        (keep (fn [[arg-ty var-name]]
                                (when-let [vty (e/lookup-env (u/env context) var-name)]
                                  (c/make-cequal arg-ty vty)))))

          new-node (n/make-recur (vec arg-nodes)
                                 (n/attrs node) (n/node-meta node) (n/parent node))]
      [nil (t/set-type! new-node nil)
       (concat arg-constraints loop-eqs)
       context])))   ;; 返回原上下文