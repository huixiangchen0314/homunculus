(ns top.kzre.homunculus.core.types.constraint.gen
  "约束生成 Pass：遍历 IR2 节点，为每个子表达式分配类型变量并收集约束。
   不执行任何合一，只生成约束列表。
   支持顺序环境处理、loop/recur、vector、let 多态泛型化、已类型化节点短路。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.constraint.model :as cm]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.types.type :as ty]))

(defn- fresh-tvar [] (t/->TVar (gensym "cg")))

(declare cg-node)

(defn generate-constraints [ir2-roots context]
  (let [state (atom {:constraints [] :env (:env context)})
        new-roots
        (mapv (fn [root]
                (let [[tv node constrs] (cg-node root (assoc context :env (:env @state)))]
                  (swap! state update :constraints into (or constrs []))
                  (when (= (ir2p/kind node) :define)
                    (swap! state update :env e/extend-env (:name node) tv))
                  node))
              ir2-roots)]
    {:roots new-roots :constraints (:constraints @state)}))

(defmulti cg-node
          "为单个 IR2 节点生成 [type-var, updated-node, constraints]"
          (fn [node context]
            ;; 只在类型为非 TVar 的具体类型时短路
            (if-let [existing (ty/get-type node)]
              (if (and (satisfies? tp/IType existing)
                       (not (ty/var-type? existing)))
                :already-typed
                (ir2p/kind node))
              (ir2p/kind node))))

(defmethod cg-node :already-typed [node context]
  [(ty/get-type node) node nil])

;; ── 字面量 ──
(defmethod cg-node :literal [node context]
  (let [frontend (:frontend context)
        ty (when frontend (tp/literal->type frontend (:val node)))
        tv (or ty (fresh-tvar))
        constraint (when ty (list (cm/->CEqual tv ty)))
        new-node (ty/set-type! node tv)]
    [tv new-node constraint]))

;; ── 变量 ──
(defmethod cg-node :variable [node context]
  (let [env (:env context)
        name (:name node)
        binding (or (e/lookup-env env name) (e/lookup-env env (symbol name)))
        frontend (:frontend context)]
    (if binding
      (let [ty (if (scheme/tscheme? binding)
                 (scheme/instantiate binding)
                 binding)]
        [ty (ty/set-type! node ty) nil])
      (if-let [meta-ty (and frontend (tp/meta->type frontend node))]
        [meta-ty (ty/set-type! node meta-ty) nil]
        (let [tv (fresh-tvar)]
          [tv (ty/set-type! node tv) nil])))))

;; ── 调用（核心）──
(defmethod cg-node :call [node context]
  (let [env (:env context)
        [fn-tv fn-node fn-constraints] (cg-node (:fn node) context)
        builtin-candidates (get-in fn-node [:attrs :builtin-fn])
        candidates (if (seq builtin-candidates) builtin-candidates fn-tv)
        args (:args node)
        results (map #(cg-node % context) args)
        arg-tys (mapv first results)
        arg-nodes (mapv second results)
        arg-constraints (mapcat #(nth % 2) results)
        ret-tv (fresh-tvar)]
    (if (and (not (satisfies? tp/IType candidates))
             (coll? candidates)
             (seq candidates)
             (not (scheme/tscheme? (first candidates))))
      [ret-tv
       (ty/set-type! node ret-tv)
       (concat (list (cm/->COverload candidates arg-tys ret-tv node))
               fn-constraints arg-constraints)]
      (let [desired (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tv (reverse arg-tys))
            new-node (m/->CallNode fn-node (vec arg-nodes) (:attrs node) (:meta node) (:parent node))]
        [ret-tv
         (ty/set-type! new-node ret-tv)
         (concat (list (cm/->CEqual fn-tv desired))
                 fn-constraints arg-constraints)]))))

;; ── let 绑定（支持多态泛型化）──
(defmethod cg-node :let [node context]
  (let [bindings (:bindings node)
        [bind-nodes new-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] [var val]]
            (let [[val-tv val-node val-constr] (cg-node val (assoc context :env env))
                  var-name (:name var)
                  ;; 如果 val 是函数类型，尝试泛型化
                  binding (if (ty/fun-type? val-tv)
                            (scheme/generalize val-tv env)
                            val-tv)
                  var-node (ty/set-type! var binding)]
              [(conj bnds [var-node val-node])
               (e/extend-env env var-name binding)
               (concat constrs val-constr)]))
          [[] (:env context) []]
          bindings)
        [body-tv body-node body-constraints] (cg-node (:body node) (assoc context :env new-env))]
    [body-tv
     (ty/set-type! (m/->LetNode (vec bind-nodes) body-node (:attrs node) (:meta node) (:parent node))
                   body-tv)
     (concat bind-constraints body-constraints)]))

;; ── 其余节点保持不变 ──
(defmethod cg-node :loop [node context]
  (let [bindings (:bindings node)
        [bind-nodes new-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] [var val]]
            (let [[val-tv val-node val-constr] (cg-node val (assoc context :env env))
                  var-name (:name var)
                  var-node (ty/set-type! var val-tv)]
              [(conj bnds [var-node val-node])
               (e/extend-env env var-name val-tv)
               (concat constrs val-constr)]))
          [[] (:env context) []]
          bindings)
        loop-var-names (mapv (fn [[v _]] (:name v)) bind-nodes)
        env-loop (assoc new-env :ir2/loop-vars loop-var-names)
        [body-tv body-node body-constr] (cg-node (:body node) (assoc context :env env-loop))
        new-node (m/->LoopNode (vec bind-nodes) body-node (:attrs node) (:meta node) (:parent node))]
    [body-tv (ty/set-type! new-node body-tv)
     (concat bind-constraints body-constr)]))

(defmethod cg-node :block [node context]
  (let [exprs (:exprs node)
        results (map #(cg-node % context) exprs)
        types (map first results)
        new-exprs (mapv second results)
        constrs (mapcat #(nth % 2) results)
        last-tv (if (seq types) (last types) (t/->TCon :nil))
        new-node (m/->BlockNode new-exprs (:attrs node) (:meta node) (:parent node))]
    [last-tv (ty/set-type! new-node last-tv) constrs]))

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
    [tv (ty/set-type! new-node tv)
     (concat test-constr then-constr else-constr test-eq branch-eq)]))

(defmethod cg-node :assign [node context]
  (let [[var-tv var-node var-constr] (cg-node (:var node) context)
        [val-tv val-node val-constr] (cg-node (:val node) context)
        tv (t/->TCon :nil)
        new-node (m/->AssignNode var-node val-node (:attrs node) (:meta node) (:parent node))]
    [tv (ty/set-type! new-node tv)
     (concat var-constr val-constr (list (cm/->CEqual var-tv val-tv)))]))

(defmethod cg-node :while [node context]
  (let [[test-tv test-node test-constr] (cg-node (:test node) context)
        [body-tv body-node body-constr] (cg-node (:body node) context)
        tv (t/->TCon :nil)
        test-eq (list (cm/->CEqual test-tv (t/->TCon :bool)))
        new-node (m/->WhileNode test-node body-node (:attrs node) (:meta node) (:parent node))]
    [tv (ty/set-type! new-node tv)
     (concat test-constr body-constr test-eq)]))

(defmethod cg-node :define [node context]
  (let [[val-tv val-node val-constr] (cg-node (:val node) context)
        new-node (m/->DefineNode (:name node) val-node (:doc node) (:attrs node) (:meta node) (:parent node))]
    [val-tv (ty/set-type! new-node val-tv) val-constr]))

(defmethod cg-node :lambda [node context]
  (let [params (:params node)
        env (:env context)
        param-tys (mapv (fn [p] (or (ty/get-type p) (fresh-tvar))) params)
        param-names (map :name params)
        new-env (reduce (fn [env [name ty]] (e/extend-env env name ty))
                        env
                        (map vector param-names param-tys))
        [body-tv body-node body-constr] (cg-node (:body node) (assoc context :env new-env))
        fn-ty (reduce (fn [ret arg] (t/->TFun arg ret)) body-tv (reverse param-tys))
        param-nodes (mapv (fn [p ty] (ty/set-type! p ty)) params param-tys)
        new-node (m/->LambdaNode param-nodes body-node (:captures node) (:fn-name node)
                                 (:attrs node) (:meta node) (:parent node))]
    [fn-ty (ty/set-type! new-node fn-ty) body-constr]))

(defmethod cg-node :recur [node context]
  (let [loop-var-names (get (:env context) :ir2/loop-vars)]
    (when-not loop-var-names
      (throw (ex-info "recur outside loop" {})))
    (let [args (:args node)
          _ (when (not= (count args) (count loop-var-names))
              (throw (ex-info "recur arg count mismatch" {})))
          results (map #(cg-node % context) args)
          arg-tys (mapv first results)
          arg-nodes (mapv second results)
          arg-constraints (mapcat #(nth % 2) results)
          loop-eqs (map (fn [arg-ty var-name]
                          (let [var-ty (e/lookup-env (:env context) var-name)]
                            (cm/->CEqual arg-ty var-ty)))
                        arg-tys loop-var-names)
          tv (t/->TCon :nil)
          new-node (m/->RecurNode (vec arg-nodes) (:attrs node) (:meta node) (:parent node))]
      [tv (ty/set-type! new-node tv)
       (concat arg-constraints loop-eqs)])))

(defmethod cg-node :vector [node context]
  (let [items (:items node)
        results (map #(cg-node % context) items)
        item-tys (mapv first results)
        item-nodes (mapv second results)
        constrs (mapcat #(nth % 2) results)
        elem-ty (if (seq item-tys) (first item-tys) (fresh-tvar))
        shape (if (every? #(= :literal (ir2p/kind %)) item-nodes)
                (t/->FixedLength (count items))
                (t/->VariableLength))
        ty-container (t/->TContainer :vector elem-ty shape)
        new-node (m/->VectorNode (vec item-nodes) (:attrs node) (:meta node) (:parent node))]
    [ty-container (ty/set-type! new-node ty-container) constrs]))

(defmethod cg-node :map [node context]
  (if-let [existing (ty/get-type node)]
    (if (ty/var-type? existing)
      (let [tv (fresh-tvar)]
        [tv (ty/set-type! node tv) nil])
      [existing node nil])
    (let [tv (fresh-tvar)]
      [tv (ty/set-type! node tv) nil])))

;; ── try / catch / throw ──
(defmethod cg-node :try [node context]
  (let [tv (fresh-tvar)]   ;; 改为分配 TVar，不再硬编码 TCon :nil
    [tv (ty/set-type! node tv) nil]))
(defmethod cg-node :catch [node context]
  (let [tv (fresh-tvar)]
    [tv (ty/set-type! node tv) nil]))
(defmethod cg-node :throw [node context]
  (let [tv (fresh-tvar)]
    [tv (ty/set-type! node tv) nil]))

(defmethod cg-node :default [node context]
  (let [tv (fresh-tvar)]
    [tv (ty/set-type! node tv) nil]))