(ns top.kzre.homunculus.core.types.ho-elim.core
  "高阶函数内联 Pass：标记并内联高阶函数。
   使用 reduce-children + walk 模式，ho-elim-fn 绝不递归子节点。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.model :as p]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.subst.replace :as replace]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.core.types.ho-elim.analyze :as analyze]
    [top.kzre.homunculus.internal.symbol :as sym]))

;; ── 环境操作 ──────────────────────────────
(defn- empty-env []
  {:defs   {}
   :ho-set #{}})

(defn- add-def [env name lam]
  (-> env
      (assoc-in [:defs name] lam)
      (update :ho-set conj name)))

;; ── 内联辅助 ──────────────────────────────
(defn- inline-call [lam args]
  (let [params (n/lambda-params lam)
        body   (n/lambda-body lam)]
    (reduce (fn [b [p a]]
              (replace/replace-var b (n/var-name p) a))
            body
            (map vector params args))))

(declare walk)

(defn- ho-elim-fn [node ctx]
  (let [depth (get ctx :depth 0)
        max-depth (get ctx :ho-max-depth 20)]
    (case (p/kind node)
      ;; define：如果 lambda 被标记为高阶，则加入环境
      :define
      (let [name (n/define-name node)
            val  (n/define-val node)
            ho?  (-> node n/attrs :ho?)]
        (if (and val ho? (n/lambda-node? val))
          (walk node (update ctx :env add-def name val))
          (walk node ctx)))

      ;; call：尝试内联高阶调用
      :call
      (let [fn-node (n/call-fn node)
            args    (n/call-args node)
            env     (:env ctx)]
        (if (and (n/variable-node? fn-node) (< depth max-depth))
          (let [fn-name (n/var-name fn-node)
                local-lam (get-in env [:defs fn-name])
                local-ho? (contains? (:ho-set env) fn-name)]
            (if (and local-ho? local-lam)
              ;; 本地高阶函数内联，返回内联结果（新节点），增加深度
              (let [inlined (inline-call local-lam args)]
                (walk inlined (update ctx :depth inc)))
              ;; 尝试全局符号表
              (if-let [global-table (:symbol-table ctx)]
                (let [entry (sym/lookup-func global-table fn-name)]
                  (if (and entry (:ho? entry) (:ir2 entry))
                    (let [lam (:ir2 entry)
                          inlined (inline-call lam args)
                          ctx' (-> ctx
                                   (update :depth inc)
                                   (update :env add-def fn-name lam))]
                      (walk inlined ctx'))
                    (walk node ctx)))
                (walk node ctx))))
          (walk node ctx)))
      (walk node ctx))))

(defn- walk [node ctx]
  (p/reduce-children node ho-elim-fn ctx))

;; ── 上下文构建 ──────────────────────────
(defn make-context
  [compile-ctx frontend backend]
  (let [builtin-table (tp/builtin-symbols frontend)
        user-table    (ip/symbol-table compile-ctx)
        symbols       (merge builtin-table user-table)]
    {:env              (empty-env)
     :frontend         frontend
     :ctx              compile-ctx
     :backend          backend
     :ho-max-depth     20
     :symbol-table     symbols
     :known-types      (sym/types-symbols symbols)
     :depth            0}))

;; ── 入口 ──
(defn process
  [ir2-roots context]
  (let [analyzed-roots (analyze/analyze ir2-roots)
        ctx (assoc context :env (empty-env) :depth 0)
        [new-roots _]
        (reduce (fn [[roots ctx] root]
                  (let [[new-root new-ctx] (ho-elim-fn root ctx)]
                    [(conj roots new-root) new-ctx]))
                [[] ctx]
                analyzed-roots)]
    new-roots))