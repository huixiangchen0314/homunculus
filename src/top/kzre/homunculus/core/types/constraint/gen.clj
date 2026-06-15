(ns top.kzre.homunculus.core.types.constraint.gen
  "约束生成 Pass：遍历 IR2 节点，为每个子表达式分配类型变量并收集约束。
   不执行任何合一，只生成约束列表。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.constraint.model :as cm]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]))

;; ── 工具：生成新鲜类型变量 ──
(defn- fresh-tvar [] (t/->TVar (gensym "cg")))

(declare cg-node)

;; ── 顶层入口：处理 IR2 根节点序列 ──
(defn generate-constraints
  "输入：ir2-roots（IR2 节点列表），env（类型环境 map）
   输出：{:roots updated-roots :constraints list}"
  [ir2-roots env]
  (let [state (atom {:constraints []})
        new-roots
        (mapv (fn [root]
                (let [[tv node constrs] (cg-node root env)]
                  (swap! state update :constraints into (or constrs []))
                  node))
              ir2-roots)]
    {:roots new-roots :constraints (:constraints @state)}))

;; ── 多方法：根据节点 kind 分派 ──
(defmulti cg-node
          "为单个 IR2 节点生成 [type-var, updated-node, constraints]"
          (fn [node context] (ir2p/kind node)))

;; ── 字面量 ──
(defmethod cg-node :literal [node context]
  (let [frontend (:frontend context)
        ty (when frontend (tp/literal->type frontend (:val node)))
        tv (or ty (fresh-tvar))
        constraint (when ty (list (cm/->CEqual tv ty)))
        new-node (assoc-in node [:attrs :type] tv)]
    [tv new-node constraint]))

;; ── 变量（修复：已知绑定直接返回类型，未知绑定创建新变量）──
(defmethod cg-node :variable [node context]
  (let [env (:env context)
        name (:name node)
        binding (or (e/lookup-env env name) (e/lookup-env env (symbol name)))]
    (if binding
      [binding (assoc-in node [:attrs :type] binding) nil]  ;; 已知绑定，无需约束
      (let [tv (fresh-tvar)]
        [tv (assoc-in node [:attrs :type] tv) nil]))))       ;; 未知变量，分配新变量

;; ── 调用（核心）──
(defmethod cg-node :call [node context]
  (let [env (:env context)
        [fn-tv fn-node fn-constraints] (cg-node (:fn node) context)
        ;; 内置函数候选（由 builtin-check 存储在 attrs 中）
        builtin-candidates (get-in fn-node [:attrs :builtin-fn])
        ;; 如果有内置候选列表则使用，否则使用推断出的 fn-tv
        candidates (if (seq builtin-candidates) builtin-candidates fn-tv)
        args (:args node)
        ;; 推断实参
        results (map #(cg-node % context) args)
        arg-tys (mapv first results)
        arg-nodes (mapv second results)
        arg-constraints (mapcat #(nth % 2) results)
        ret-tv (fresh-tvar)]
    (if (and (not (satisfies? tp/IType candidates))
             (coll? candidates)
             (seq candidates)
             (not (scheme/tscheme? (first candidates))))
      ;; 候选列表 → 重载约束
      [ret-tv
       (assoc-in node [:attrs :type] ret-tv)
       (concat (list (cm/->COverload candidates arg-tys ret-tv node))
               fn-constraints arg-constraints)]
      ;; 单一类型或变量 → 等式约束 (fn-tv = arg1 -> arg2 -> ret)
      (let [desired (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tv (reverse arg-tys))]
        [ret-tv
         (assoc-in (m/->CallNode fn-node (vec arg-nodes) (:attrs node) (:meta node) (:parent node))
                   [:attrs :type] ret-tv)
         (concat (list (cm/->CEqual fn-tv desired))
                 fn-constraints arg-constraints)]))))

;; ── let 绑定 ──
(defmethod cg-node :let [node context]
  (let [bindings (:bindings node)
        [bind-nodes new-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] [var val]]
            (let [[val-tv val-node val-constr] (cg-node val (assoc context :env env))
                  var-name (:name var)
                  var-node (assoc-in var [:attrs :type] val-tv)]
              [(conj bnds [var-node val-node]) (e/extend-env env var-name val-tv) (concat constrs val-constr)]))
          [[] (:env context) []]
          bindings)
        [body-tv body-node body-constraints] (cg-node (:body node) (assoc context :env new-env))]
    [body-tv
     (assoc-in (m/->LetNode (vec bind-nodes) body-node (:attrs node) (:meta node) (:parent node))
               [:attrs :type] body-tv)
     (concat bind-constraints body-constraints)]))

;; ── if ──
(defmethod cg-node :if [node context]
  (let [[test-tv test-node test-constr] (cg-node (:test node) context)
        [then-tv then-node then-constr] (cg-node (:then node) context)
        [else-tv else-node else-constr] (if (:else node)
                                          (cg-node (:else node) context)
                                          [nil nil nil])
        tv (fresh-tvar)
        test-eq (when test-tv (list (cm/->CEqual test-tv (t/->TCon :bool))))
        branch-eq (if else-tv
                    [(cm/->CEqual then-tv tv) (cm/->CEqual else-tv tv)]
                    [(cm/->CEqual then-tv tv)])
        new-node (m/->IfNode test-node then-node else-node (:attrs node) (:meta node) (:parent node))]
    [tv (assoc-in new-node [:attrs :type] tv)
     (concat test-constr then-constr else-constr test-eq branch-eq)]))

;; ── block ──
(defmethod cg-node :block [node context]
  (let [exprs (:exprs node)
        results (map #(cg-node % context) exprs)
        types (map first results)
        new-exprs (mapv second results)
        constrs (mapcat #(nth % 2) results)
        last-tv (last types)
        new-node (m/->BlockNode new-exprs (:attrs node) (:meta node) (:parent node))]
    [last-tv (assoc-in new-node [:attrs :type] last-tv) constrs]))

;; ── assign ──
(defmethod cg-node :assign [node context]
  (let [[var-tv var-node var-constr] (cg-node (:var node) context)
        [val-tv val-node val-constr] (cg-node (:val node) context)
        tv (t/->TCon :nil)   ; assign 自身类型为 nil
        new-node (m/->AssignNode var-node val-node (:attrs node) (:meta node) (:parent node))]
    [tv (assoc-in new-node [:attrs :type] tv)
     (concat var-constr val-constr (list (cm/->CEqual var-tv val-tv)))]))

;; ── while ──
(defmethod cg-node :while [node context]
  (let [[test-tv test-node test-constr] (cg-node (:test node) context)
        [body-tv body-node body-constr] (cg-node (:body node) context)
        tv (t/->TCon :nil)
        test-eq (list (cm/->CEqual test-tv (t/->TCon :bool)))
        new-node (m/->WhileNode test-node body-node (:attrs node) (:meta node) (:parent node))]
    [tv (assoc-in new-node [:attrs :type] tv)
     (concat test-constr body-constr test-eq)]))

;; ── define ──
(defmethod cg-node :define [node context]
  (let [[val-tv val-node val-constr] (cg-node (:val node) context)
        new-node (m/->DefineNode (:name node) val-node (:doc node) (:attrs node) (:meta node) (:parent node))]
    [val-tv (assoc-in new-node [:attrs :type] val-tv) val-constr]))

;; ── lambda ──
(defmethod cg-node :lambda [node context]
  (let [params (:params node)
        env (:env context)
        param-tys (mapv (fn [p] (or (get-in p [:attrs :type]) (fresh-tvar))) params)
        param-names (map :name params)
        new-env (reduce (fn [env [name ty]] (e/extend-env env name ty))
                        env
                        (map vector param-names param-tys))
        [body-tv body-node body-constr] (cg-node (:body node) (assoc context :env new-env))
        fn-ty (reduce (fn [ret arg] (t/->TFun arg ret)) body-tv (reverse param-tys))
        param-nodes (mapv (fn [p ty] (assoc-in p [:attrs :type] ty)) params param-tys)
        new-node (m/->LambdaNode param-nodes body-node (:captures node) (:fn-name node)
                                 (:attrs node) (:meta node) (:parent node))]
    [fn-ty (assoc-in new-node [:attrs :type] fn-ty) body-constr]))

;; ── 默认（其他节点暂时只分配类型变量）──
(defmethod cg-node :default [node context]
  (let [tv (fresh-tvar)]
    [tv (assoc-in node [:attrs :type] tv) nil]))