(ns top.kzre.homunculus.core.types.inline.core
  "基于标记的内联 Pass：处理用户标记的 :inline 函数以及多态函数的内联。
   上下文通过 :inline-polymorphic? 控制是否强制内联多态函数。
   内联后清除类型信息，以便后续类型推导基于具体上下文重新进行。
   本模块内定义的 inline/polymorphic 函数会被累积到局部环境，供后续节点使用。"
  (:require
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.subst.replace :as replace]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.internal.symbol :as sym]
    [top.kzre.homunculus.internal.protocol :as ip]))

;; ── 构建上下文 ──────────────────────────
(defn make-context
  "构造内联上下文。
   compile-ctx : 编译上下文
   frontend    : 前端协议实例
   backend     : 后端协议实例
   opts        : 可选参数，当前支持 :inline-polymorphic? (默认 true)"
  [compile-ctx frontend backend & {:keys [inline-polymorphic?] :or {inline-polymorphic? true}}]
  (let [builtin-table (tp/builtin-symbols frontend)
        user-table    (ip/symbol-table compile-ctx)
        symbols       (merge builtin-table user-table)]
    {:env {:symbol-table symbols}          ;; 全局符号表，内置 + 编译上下文
     :frontend frontend
     :ctx compile-ctx
     :backend backend
     :inline-polymorphic? inline-polymorphic?
     :known-types (sym/types-symbols symbols)
     :local-inline-defs {}}))              ;; 局部内联函数定义 {fn-name -> lambda-node}

;; ── 内联辅助函数 ─────────────────────────
(defn- strip-types [node]
  (clojure.walk/prewalk
    (fn [x]
      (if (satisfies? ir2p/INode x)
        (n/set-type-attr x nil)
        x))
    node))

(defn- find-lambda-to-inline [fn-name ctx]
  "查找可用于内联的函数体（lambda 节点）。先查局部定义，再查全局符号表。"
  (or (get-in ctx [:local-inline-defs fn-name])
      (when-let [entry (get-in ctx [:env :symbol-table fn-name])]
        (when (or (:inline entry)
                  (and (:polymorphic entry)
                       (:inline-polymorphic? ctx)))
          (:ir2 entry)))))

(defn- inline-call
  "根据函数节点和实参列表进行内联，返回内联后的新节点（已清除类型）。"
  [fn-node args node ctx]
  (let [fn-name (when (= (n/kind fn-node) :variable)
                  (n/var-name fn-node))
        lam     (when fn-name (find-lambda-to-inline fn-name ctx))]
    (if lam
      (let [params  (n/lambda-params lam)
            body    (n/lambda-body lam)
            inlined (reduce (fn [b [p a]]
                              (replace/replace-var b (n/var-name p) a))
                            body
                            (map vector params args))]
        (strip-types inlined))
      (n/make-call fn-node args (n/attrs node) (n/node-meta node) (n/parent node)))))

;; ── 环境更新 ──────────────────────────
(defn- add-inline-def [ctx node]
  "如果是 define 且包含 :inline 或 :polymorphic 标记，则将其 lambda 存入局部环境。"
  (if (and (= (n/kind node) :define)
           (n/define-val node)
           (= (n/kind (n/define-val node)) :lambda))
    (let [attrs (n/attrs node)
          inline? (true? (:inline attrs))
          polym?  (true? (:polymorphic attrs))]
      (if (or inline? polym?)
        (let [fn-name (n/define-name node)
              lam     (n/define-val node)]
          (update ctx :local-inline-defs assoc fn-name lam))
        ctx))
    ctx))

;; ── 多方法遍历 ──────────────────────────
(defmulti inline-node
          (fn [node ctx] (n/kind node)))

(defmethod inline-node :call [node ctx]
  (let [fn-node (n/call-fn node)
        args    (n/call-args node)
        new-args (mapv #(inline-node % ctx) args)
        new-fn   (inline-node fn-node ctx)]
    (inline-call new-fn new-args node ctx)))

(defmethod inline-node :block [node ctx]
  (n/make-block (mapv #(inline-node % ctx) (n/block-exprs node))
                (n/attrs node)
                (n/node-meta node)
                (n/parent node)))

(defmethod inline-node :let [node ctx]
  (n/make-let (mapv (fn [[v e]] [(inline-node v ctx) (inline-node e ctx)]) (n/let-bindings node))
              (inline-node (n/let-body node) ctx)
              (n/attrs node)
              (n/node-meta node)
              (n/parent node)))

(defmethod inline-node :loop [node ctx]
  (n/make-loop (mapv (fn [[v e]] [(inline-node v ctx) (inline-node e ctx)]) (n/loop-bindings node))
               (inline-node (n/loop-body node) ctx)
               (n/attrs node)
               (n/node-meta node)
               (n/parent node)))

(defmethod inline-node :if [node ctx]
  (n/make-if (inline-node (n/if-test node) ctx)
             (inline-node (n/if-then node) ctx)
             (when-let [e (n/if-else node)] (inline-node e ctx))
             (n/attrs node)
             (n/node-meta node)
             (n/parent node)))

(defmethod inline-node :while [node ctx]
  (n/make-while (inline-node (n/while-test node) ctx)
                (inline-node (n/while-body node) ctx)
                (n/attrs node)
                (n/node-meta node)
                (n/parent node)))

(defmethod inline-node :assign [node ctx]
  (n/make-assign (inline-node (n/assign-var node) ctx)
                 (inline-node (n/assign-val node) ctx)
                 (n/attrs node)
                 (n/node-meta node)
                 (n/parent node)))

(defmethod inline-node :lambda [node ctx]
  (n/make-lambda (n/lambda-params node)
                 (inline-node (n/lambda-body node) ctx)
                 (n/lambda-captures node)
                 (n/lambda-fn-name node)
                 (n/attrs node)
                 (n/node-meta node)
                 (n/parent node)))

(defmethod inline-node :recur [node ctx]
  (n/make-recur (mapv #(inline-node % ctx) (n/recur-args node))
                (n/attrs node)
                (n/node-meta node)
                (n/parent node)))

(defmethod inline-node :define [node ctx]
  (n/make-define (n/define-name node)
                 (inline-node (n/define-val node) ctx)
                 (n/define-doc node)
                 (n/attrs node)
                 (n/node-meta node)
                 (n/parent node)))

;; 数组特殊节点
(defmethod inline-node :new-array [node ctx]
  (n/make-new-array (inline-node (n/new-array-size node) ctx)
                    (n/node-meta node)
                    (n/parent node)))
(defmethod inline-node :aget [node ctx]
  (n/make-aget (inline-node (n/aget-target node) ctx)
               (inline-node (n/aget-idx node) ctx)
               (n/node-meta node)
               (n/parent node)))
(defmethod inline-node :aset [node ctx]
  (n/make-aset (inline-node (n/aset-target node) ctx)
               (inline-node (n/aset-idx node) ctx)
               (inline-node (n/aset-val node) ctx)
               (n/node-meta node)
               (n/parent node)))
(defmethod inline-node :alength [node ctx]
  (n/make-alength (inline-node (n/alength-target node) ctx)
                  (n/node-meta node)
                  (n/parent node)))

;; 默认：其他节点直接返回
(defmethod inline-node :default [node _] node)

;; ── 入口 ──
(defn process
  "对 IR2 根节点列表执行内联。顺序处理根节点，累积本模块内定义的 inline/polymorphic 函数。"
  [ir2-roots context]
  (let [ctx-with-local (assoc context :local-inline-defs {})]
    (first
      (reduce (fn [[roots ctx] root]
                (let [ctx' (add-inline-def ctx root)
                      new-root (inline-node root ctx')]
                  [(conj roots new-root) ctx']))
              [[] ctx-with-local]
              ir2-roots))))