(ns top.kzre.homunculus.core.types.constraint.gen.core
  "约束生成 Pass 的核心调度：多方法定义与主入口。
   同时负责为已有具体类型标注的节点生成验证约束。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.constraint.model :as cm]
            [top.kzre.homunculus.core.types.type :as ty]))

;; ── 工具函数 ────────────────────────────
(defn fresh-tvar []
  (t/->TVar (gensym "cg")))

;; ── 底层多方法（仅按节点 kind 分发，不做任何标注短路）──
(declare cg-node-raw)

(defmulti cg-node-raw
          "为单个 IR2 节点生成约束的核心多方法。返回 [type-var new-node constraints]。
           不再自行处理已有的具体类型，统一交给外部包装层。"
          (fn [node _context] (ir2p/kind node)))

(defmethod cg-node-raw :default [node _context]
  (let [tv (fresh-tvar)]
    [tv (ty/set-type! node tv) nil]))

;; ── 包装函数：自动为有类型标注的节点附加验证约束 ──
(defn cg-node
  "节点约束生成入口。
   1. 调用 cg-node-raw 得到推导类型 tv、新节点、内部约束。
   2. 若原始节点已有具体类型标注（非 TVar），生成 (CEqual tv annotated-ty) 验证约束。
   3. 返回合并后的 [tv new-node constraints]。"
  [node context]
  (let [annotated-ty (ty/get-type node)   ;; 推导前提取标注类型
        [tv new-node inner-constrs] (cg-node-raw node context)]
    (if (and annotated-ty
             (satisfies? tp/IType annotated-ty)
             (not (ty/var-type? annotated-ty)))
      ;; 存在具体标注，追加验证约束
      [tv new-node (concat inner-constrs [(cm/->CEqual tv annotated-ty)])]
      [tv new-node inner-constrs])))

;; ── 全局入口 ────────────────────────────
(defn generate-constraints
  "给定 IR2 节点树，为其生成约束。
   返回新的节点树和约束域。"
  [ir2-roots context]
  (let [state (atom {:constraints []
                     :env (:env context)})
        new-roots
        (mapv (fn [root]
                (let [[tv node constrs] (cg-node root (assoc context :env (:env @state)))]
                  (swap! state update :constraints into (or constrs []))
                  (when (= (ir2p/kind node) :define)
                    (swap! state update :env e/extend-env (:name node) tv))
                  node))
              ir2-roots)]
    {:roots new-roots
     :constraints (:constraints @state)}))