(ns top.kzre.homunculus.core.types.constraint.gen.core
  "约束生成 Pass 的核心调度：多方法定义与主入口。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.type :as ty]))

(defn frontend [context] (:frontend context))
(defn frontend-types [context] (tp/frontend-types (frontend context)))

(defn fresh-tvar [] (ty/make-tvar (gensym "cg")))

(declare cg-node-raw)

(defmulti cg-node-raw
          (fn [node _context] (n/kind node)))

(defmethod cg-node-raw :default [node _context]
  (let [tv (fresh-tvar)]
    [tv (ty/set-type! node tv) nil]))

;; ── 包装函数：自动将三元组升级为四元组（上下文为 nil）──
(defn cg-node
  "节点约束生成入口，返回 [tv new-node constraints new-context-or-nil]。"
  [node context]
  (let [known-types (frontend-types context)
        annotated-ty (ty/get-type node known-types)
        raw (cg-node-raw node context)
        ;; 确保至少是三元组
        [tv new-node inner-constrs] raw
        ;; 如果 raw 长度 >=4，取第4个作为 new-ctx，否则 nil
        new-ctx (when (>= (count raw) 4) (nth raw 3))]
    (if (and annotated-ty
             (satisfies? tp/IType annotated-ty)
             (not (ty/var-type? annotated-ty)))
      [tv new-node (concat inner-constrs [(c/make-cequal tv annotated-ty)]) new-ctx]
      [tv new-node inner-constrs new-ctx])))

;; ── 全局入口 ──
(defn generate-constraints [ir2-roots context]
  (let [state (atom {:constraints []
                     :env (:env context)})
        new-roots
        (mapv (fn [root]
                (let [[tv node constrs new-ctx] (cg-node root (assoc context :env (:env @state)))]
                  (swap! state update :constraints into (or constrs []))
                  ;; 如果返回了有效的新上下文，则将其环境合并到全局
                  (when new-ctx
                    (swap! state assoc :env (:env new-ctx)))
                  node))
              ir2-roots)]
    {:roots new-roots
     :constraints (:constraints @state)}))