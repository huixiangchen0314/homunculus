(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.core
  "约束生成 Pass 的核心调度：多方法定义与主入口。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

;; ── 类型变量生成 ──────────────────────────
(defn fresh-tvar [] (ty/make-tvar (gensym "cg")))

(defn result
  [type node env & constraints]
  {:type        type
   ;; 尊重已有推断
   :node        (ty/ensure-type node type)
   :constraints (vec (keep identity (flatten constraints)))
   :env         env})

;; ── 内部多方法 ──────────────────────────
(declare gen-node*)

(defmulti gen-node*
          (fn [node _env] (n/kind node)))

(defmethod gen-node* :default [node env]
  (let [tv (or (ty/get-type node) (fresh-tvar))]
    (result tv node env)))

(defn gen-node-hinted
  "节点约束生成入口，返回 {:type :node :constraints :env}。
   若节点已有注解类型，附加一条 type ≡ annotated 约束。"
  [raw-node current-env]
  (let [hinted-type (ty/get-type raw-node)
        {:keys [type] :as r} (gen-node* raw-node current-env)]
    (if (ty/concrete? hinted-type)
      (update r :constraints conj (cons/make-cequal type hinted-type))
      r)))

(defn gen-seq [nodes env]
  (reduce (fn [[results current-env] arg]
            (let [{:keys [env] :as result} (gen-node-hinted arg current-env)]
              [(conj results result) env]))
          [[] env]
          nodes))


;; ── 全局入口 ────────────────────────────
(defn gen
  "对 IR2 根节点列表做约束生成。
   返回 {:roots :nodes :constraints}。"
  [ir2-roots init-env]
  (let [[new-roots constraints _final-env]
        (reduce
          (fn [[roots constrs current-env] current-node]
            (let [{:keys [node constraints env]} (gen-node-hinted current-node current-env)]
              [(conj roots node)
               (into constrs constraints)
               env]))
          [[] [] init-env]
          ir2-roots)]
    {:roots       new-roots
     :nodes       new-roots
     :constraints constraints}))