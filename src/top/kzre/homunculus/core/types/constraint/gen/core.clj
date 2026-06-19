(ns top.kzre.homunculus.core.types.constraint.gen.core
  "约束生成 Pass 的核心调度：多方法定义与主入口。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.constraint.constraint :as c]
    [top.kzre.homunculus.core.types.constraint.utils :as u]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.type :as ty]))

;; ── 类型变量生成 ──────────────────────────
(defn fresh-tvar [] (ty/make-tvar (gensym "cg")))

;; ── 多方法分发 ──────────────────────────
(declare cg-node-raw)

(defmulti cg-node-raw
          (fn [node _context] (n/kind node)))

(defmethod cg-node-raw :default [node context]
  ;; 默认生成新类型变量，无约束，上下文不变
  (let [tv (fresh-tvar)]
    [tv (ty/set-type! node tv) nil context]))

;; ── 包装函数 ────────────────────────────
(defn cg-node
  "节点约束生成入口，返回 [tv new-node constraints new-context]。"
  [node context]
  (let [known-types (u/known-types context)
        annotated-ty (ty/get-type node known-types)
        ;; cg-node-raw 保证返回四元组
        [tv new-node inner-constrs new-ctx] (cg-node-raw node context)]
    (if (and annotated-ty
             (satisfies? tp/IType annotated-ty)
             (not (ty/var-type? annotated-ty)))
      [tv new-node (concat inner-constrs [(c/make-cequal tv annotated-ty)]) new-ctx]
      [tv new-node inner-constrs new-ctx])))

;; ── 全局入口：用 reduce 顺序处理，直接传递环境 ──
(defn generate-constraints [ir2-roots context]
  (let [initial-acc [[] [] context]  ;; [new-roots, constraints, current-context]
        [new-roots constraints _final-ctx]
        (reduce
          (fn [[roots constrs ctx] node]
            (let [[_tv new-node node-constrs new-ctx] (cg-node node ctx)]
              [(conj roots new-node)
               (into constrs (or node-constrs []))
               new-ctx]))
          initial-acc
          ir2-roots)]
    {:roots new-roots
     :constraints constraints}))