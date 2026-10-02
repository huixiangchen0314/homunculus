(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.core
  "约束生成 Pass 的核心调度：多方法定义与主入口。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

;; ── 类型变量生成 ──────────────────────────
(defn fresh-tvar [] (ty/make-tvar (gensym "cg")))

;; ── 结果构造 ────────────────────────────
(defn result
  "约束生成的统一返回值。
   type        推断出的类型
   node        重建后的节点
   constraints 累积的约束列表
   env         更新后的环境"
  [type node constraints env]
  {:type        type
   :node        node
   :constraints (or constraints [])
   :env         env})

;; ── 内部多方法 ──────────────────────────
(declare gen-node*)

(defmulti gen-node*
          (fn [node _context] (n/kind node)))

(defmethod gen-node* :default [node context]
  (let [tv (fresh-tvar)]
    (result tv (ty/set-type! node tv) nil context)))

;; ── 对外单节点入口 ──────────────────────
(defn gen-node
  "节点约束生成入口，返回 {:type :node :constraints :env}。
   若节点已有注解类型，附加一条 type ≡ annotated 约束。"
  [current-node current-context]
  (let [hinted-type (ty/get-type current-node)
        {:keys [type] :as r} (gen-node* current-node current-context)]
    (if (and hinted-type
             (satisfies? tp/IType hinted-type)
             (not (ty/var-type? hinted-type)))
      (update r :constraints conj (cons/make-cequal type hinted-type))
      r)))

;; ── 顺序遍历辅助 ────────────────────────
(defn gen-nodes
  "顺序约束生成一组节点（走 gen-node，含注解处理）。
   返回 {:results [...] :env ...}。"
  [nodes context]
  (reduce (fn [acc node]
            (let [r (gen-node node (:env acc))]
              (-> acc
                  (update :results conj r)
                  (assoc :env (:env r)))))
          {:results [] :env context}
          nodes))

(defn gen-nodes*
  "同 gen-nodes，但走 gen-node*（跳过注解处理）。"
  [nodes context]
  (reduce (fn [acc node]
            (let [r (gen-node* node (:env acc))]
              (-> acc
                  (update :results conj r)
                  (assoc :env (:env r)))))
          {:results [] :env context}
          nodes))

;; ── 全局入口 ────────────────────────────
(defn gen
  "对 IR2 根节点列表做约束生成。
   返回 {:roots :nodes :constraints}。"
  [ir2-roots init-env]
  (let [[new-roots constraints _final-env]
        (reduce
          (fn [[roots constrs current-env] current-node]
            (let [{:keys [node constraints env]} (gen-node current-node current-env)]
              [(conj roots node)
               (into constrs constraints)
               env]))
          [[] [] init-env]
          ir2-roots)]
    {:roots       new-roots
     :nodes       new-roots
     :constraints constraints}))