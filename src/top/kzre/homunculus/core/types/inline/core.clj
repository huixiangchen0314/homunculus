(ns top.kzre.homunculus.core.types.inline.core
  "基于标记的内联 Pass：处理用户标记的 :inline 函数以及多态函数的内联。
   使用 reduce-children 统一递归，无需手写遍历。"
  (:require
   [clojure.walk :as walk]
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.core.ir2.ast :as p]
   [top.kzre.homunculus.core.types.protocol :as tp]
   [top.kzre.homunculus.core.types.subst.replace :as replace]
   [top.kzre.homunculus.internal.protocol :as ip]
   [top.kzre.homunculus.internal.symbol :as sym]))

;; ── 构建上下文 ──────────────────────────
(defn make-context
  [compile-ctx frontend backend & {:keys [inline-polymorphic?] :or {inline-polymorphic? true}}]
  (let [builtin-table (tp/builtin-symbols frontend)
        user-table    (ip/symbol-table compile-ctx)
        symbols       (merge builtin-table user-table)]
    {:symbol-table symbols
     :frontend frontend
     :ctx compile-ctx
     :backend backend
     :inline-polymorphic? inline-polymorphic?
     :known-types (sym/types-symbols symbols)
     :local-inline-defs {}}))

;; ── 内联辅助函数 ─────────────────────────
(defn- strip-types [node]
  (walk/prewalk
    (fn [x]
      (if (p/ir2? x)
        (n/set-type-attr x nil)
        x))
    node))

(defn- find-lambda-to-inline [fn-name ctx]
  (or (get-in ctx [:local-inline-defs fn-name])
      (when-let [entry (get-in ctx [:symbol-table fn-name])]
        (when (or (:inline entry)
                  (and (:polymorphic entry) (:inline-polymorphic? ctx)))
          (:ir2 entry)))))

(defn- inline-call
  [fn-node args node ctx]
  (let [fn-name (when (= (p/kind fn-node) :variable)
                  (n/var-name fn-node))
        lam     (when fn-name (find-lambda-to-inline fn-name ctx))]
    (if lam
      (let [params (n/lambda-params lam)
            body   (n/lambda-body lam)
            inlined (reduce (fn [b [p a]]
                              (replace/replace-var b (n/var-name p) a))
                            body
                            (map vector params args))]
        (strip-types inlined))
      (n/make-call fn-node args (n/attrs node) (n/node-meta node)))))

(defn- add-inline-def [ctx node]
  (if (and (= (p/kind node) :define)
           (n/define-val node)
           (= (p/kind (n/define-val node)) :lambda))
    (let [attrs (n/attrs node)]
      (if (or (true? (:inline attrs))
              (true? (:polymorphic attrs)))
        (assoc-in ctx [:local-inline-defs (n/define-name node)] (n/define-val node))
        ctx))
    ctx))

;; ── 核心遍历函数 ────────────────────────
(declare walk)
(defn- inline-fn
  [node ctx]
  (case (p/kind node)
    :call
    (let [fn-node (n/call-fn node)
          args    (n/call-args node)
          inlined (inline-call fn-node args node ctx)]
      ;; 对替换后的节点继续遍历，以处理可能出现的嵌套内联
      (walk inlined ctx))

    :define
    (let [val (n/define-val node)
          ;; 先递归内联 val
          [new-val new-ctx] (if val (inline-fn val ctx) [nil ctx])
          ;; 更新环境（加入本 define 定义的内联函数）
          final-ctx (add-inline-def (assoc ctx :local-inline-defs (:local-inline-defs new-ctx))
                                    (n/make-define (n/define-name node) new-val
                                                   (n/define-docstring node) (n/attrs node)
                                                   (n/node-meta node) ))]
      ;; 返回新的 define 节点和最终环境
      [(n/make-define (n/define-name node) new-val
                      (n/define-docstring node) (n/attrs node)
                      (n/node-meta node) )
       final-ctx])

    ;; 其他节点：原样返回，由 reduce-children 递归子节点
    (walk node ctx)))


(defn walk
  [node ctx]
  (p/reduce-children node inline-fn ctx))

;; ── 入口 ──
(defn inline-nodes
  "对 IR2 根节点列表执行内联。"
  [ir2-roots context]
  (let [ctx (assoc context :local-inline-defs {})]
    (first
      (reduce (fn [[roots ctx] root]
                (let [[new-root new-ctx] (inline-fn root ctx)]
                  [(conj roots new-root) new-ctx]))
              [[] ctx]
              ir2-roots))))