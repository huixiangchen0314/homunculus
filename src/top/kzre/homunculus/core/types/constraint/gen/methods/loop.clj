;; 文件：top/kzre/homunculus/core/types/constraint/gen/methods/loop.clj
;; 合并了 loop 与 recur 的约束生成，避免循环依赖
(ns top.kzre.homunculus.core.types.constraint.gen.methods.loop
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.constraint.model :as cm]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.type :as ty]))

;; ── loop 节点约束生成 ──
(defmethod gen/cg-node-raw :loop [node context]
  (let [bindings (:bindings node)
        ;; 为每个绑定创建全新的类型变量，避免过早实例化
        [bind-nodes new-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] [var val]]
            (let [[val-tv val-node val-constr] (gen/cg-node-raw val (assoc context :env env))
                  var-name (:name var)
                  ;; 创建独立的 binding-tv，与初始值类型解耦
                  binding-tv (gen/fresh-tvar)
                  init-constr (cm/->CEqual binding-tv val-tv)   ;; 初始值类型与 binding-tv 统一
                  var-node (ty/set-type! var binding-tv)]
              [(conj bnds [var-node val-node])
               (e/extend-env env var-name binding-tv)
               (concat constrs val-constr (list init-constr))]))
          [[] (:env context) []]
          bindings)
        ;; 记录 loop 变量名，供 recur 使用
        loop-var-names (mapv (fn [[v _]] (:name v)) bind-nodes)
        env-loop (assoc new-env :ir2/loop-vars loop-var-names)
        ;; 在包含 loop 变量的环境中推导 body
        [body-tv body-node body-constr] (gen/cg-node-raw (:body node) (assoc context :env env-loop))
        new-node (m/->LoopNode (vec bind-nodes) body-node (:attrs node) (:meta node) (:parent node))]
    [body-tv (ty/set-type! new-node body-tv)
     (concat bind-constraints body-constr)]))

;; ── recur 节点约束生成 ──
(defmethod gen/cg-node-raw :recur [node context]
  (let [loop-var-names (get (:env context) :ir2/loop-vars)]
    (when-not loop-var-names
      (throw (ex-info "recur outside loop" {})))
    (let [args (:args node)
          _ (when (not= (count args) (count loop-var-names))
              (throw (ex-info "recur arg count mismatch" {})))
          results (mapv #(gen/cg-node-raw % context) args)   ;; 急切求值
          arg-tys (mapv first results)
          arg-nodes (mapv second results)
          arg-constraints (mapcat #(nth % 2) results)
          ;; 生成 recur 实参与对应 loop 变量的相等约束
          loop-eqs (->> (map vector arg-tys loop-var-names)
                        (keep (fn [[arg-ty var-name]]
                                (when-let [vty (e/lookup-env (:env context) var-name)]
                                  (cm/->CEqual arg-ty vty)))))
          tv (t/->TCon :nil)   ;; recur 本身不产生有意义的值
          new-node (m/->RecurNode (vec arg-nodes) (:attrs node) (:meta node) (:parent node))]
      [tv (ty/set-type! new-node tv)
       (concat arg-constraints loop-eqs)])))