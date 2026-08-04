(ns top.kzre.homunculus.core.types.fold.propagate
  "常量传播：基于已折叠的常量信息，将变量引用替换为字面量。
   环境包含 :constants 和 :array-lens 两个映射，不依赖类型推导。
   对 :alength 节点直接替换为已知长度。
   传播时依据 attrs 中的 :mutable 标记，只对不可变变量进行常量传播。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.ast :as p]))

;; ── 环境操作 ──────────────────────────────
(defn- make-empty-env []
  {:constants {}   ; 变量名 → 字面量节点
   :array-lens {}}) ; 数组变量名 → 长度整数

(defn- env-get-constant [env var-name]
  (get-in env [:constants var-name]))

(defn- env-get-array-len [env var-name]
  (get-in env [:array-lens var-name]))

(defn- env-add-constant [env var-name val-node]
  (assoc-in env [:constants var-name] val-node))

(defn- env-add-array-len [env var-name len]
  (assoc-in env [:array-lens var-name] len))

(defn- env-remove-var [env var-name]
  (-> env
      (update :constants dissoc var-name)
      (update :array-lens dissoc var-name)))

(defn- env-remove-vars [env var-names]
  (reduce env-remove-var env var-names))

;; ★ 新增：读取变量的可变性标记
(defn- var-mutable?
  "判断一个变量节点是否被 mutable/analyze 标记为可变。"
  [var-node]
  (true? (:mutable (n/attrs var-node))))

(defn- maybe-subst-constant [node env]
  (if (and (n/variable-node? node)
           (contains? (:constants env) (n/var-name node)))
    (get-in env [:constants (n/var-name node)])
    node))

;; ── 工具函数：将常量信息注入环境 ──────────
(defn collect-env
  "根据 var-name 和 val-node 更新环境。
   支持两种情况：
   1. val-node 是字面量节点 → 记录到 :constants
   2. val-node 是 new-array 节点且 size 为字面量 → 记录数组长度到 :array-lens"
  [env var-name val-node]
  (cond
    ;; 标量字面量
    (n/literal-node? val-node)
    (env-add-constant env var-name val-node)

    ;; new-array 且 size 为字面量整数
    (and (= :new-array (n/kind val-node))
         (n/literal-node? (n/new-array-size val-node))
         (integer? (n/lit-val (n/new-array-size val-node))))
    (env-add-array-len env var-name (n/lit-val (n/new-array-size val-node)))

    ;; 其他情况不更新
    :else env))

;; ── 多方法分派 ──────────────────────────
(defmulti propagate-node
          "返回 [new-node, updated-context]"
          (fn [node _context] (n/kind node)))


;; ── let：根据 :mutable 标记决定是否收集常量 ──
(defmethod propagate-node :let [node context]
  (let [bindings (n/let-bindings node)           ;; 返回 Binding 向量
        [new-bindings ctx1]
        (reduce (fn [[bnds ctx] b]
                  (let [var-node (:var b)
                        val-expr (:val b)
                        subbed (maybe-subst-constant val-expr (:env ctx))
                        [new-val new-ctx] (propagate-node subbed ctx)
                        ;; 仅在变量不可变时将常量加入环境
                        env' (if (var-mutable? var-node)
                               (:env new-ctx)
                               (collect-env (:env new-ctx) (:name var-node) new-val))
                        new-b (assoc b :val new-val)]
                    [(conj bnds new-b)
                     (assoc new-ctx :env env')]))
                [[] context]
                bindings)
        [new-body ctx2] (propagate-node (n/let-body node) ctx1)]
    [(n/make-let new-bindings new-body (n/attrs node) (n/node-meta node))
     ctx2]))

;; ── loop：循环变量已由 mutable 分析标记为可变，此处统一按标记处理；同时移除循环变量以遮蔽外层同名常量 ──
(defmethod propagate-node :loop [node context]
  (let [bindings (n/loop-bindings node)             ;; 现在返回 Binding 向量
        [new-bindings ctx1]
        (reduce (fn [[bnds ctx] b]                  ;; b 为 Binding 节点
                  (let [var-node (:var b)
                        val-expr (:val b)
                        subbed (maybe-subst-constant val-expr (:env ctx))
                        [new-val new-ctx] (propagate-node subbed ctx)
                        env' (if (var-mutable? var-node)
                               (:env new-ctx)
                               (collect-env (:env new-ctx) (:name var-node) new-val))
                        new-b (assoc b :val new-val)]
                    [(conj bnds new-b)
                     (assoc new-ctx :env env')]))
                [[] context]
                bindings)
        ;; 循环体内移除所有循环变量名，遮蔽外层同名常量
        loop-var-names (set (map #(:name (:var %)) bindings))
        env-body (env-remove-vars (:env ctx1) loop-var-names)
        ctx-body (assoc ctx1 :env env-body)
        [new-body ctx2] (propagate-node (n/loop-body node) ctx-body)]
    [(n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node))
     (assoc ctx2 :env (:env context))]))

;; ── define：顶级定义视为不可变，仍收集常量 ──
(defmethod propagate-node :define [node context]
  (if-let [val (n/define-val node)]
    (let [subbed (maybe-subst-constant val (:env context))
          [new-val ctx1] (propagate-node subbed context)
          env' (collect-env (:env ctx1) (n/define-name node) new-val)]
      [(n/make-define (n/define-name node) new-val (n/define-docstring node)
                      (n/attrs node) (n/node-meta node) )
       (assoc ctx1 :env env')])
    [node context]))

;; ── assign：左值不替换，右值替换，并从环境中移除被赋值的变量（因其可变）──
(defmethod propagate-node :assign [node context]
  (let [new-var (n/assign-var node)
        subbed-val (maybe-subst-constant (n/assign-val node) (:env context))
        [new-val ctx1] (propagate-node subbed-val context)
        env (if (n/variable-node? new-var)
              (env-remove-var (:env ctx1) (n/var-name new-var))
              (:env ctx1))
        ctx2 (assoc ctx1 :env env)]
    [(n/make-assign new-var new-val (n/attrs node) (n/node-meta node) )
     ctx2]))

;; ── call：普通调用，仅实参替换 ──
(defmethod propagate-node :call [node context]
  (let [new-fn (n/call-fn node)
        [new-args ctx1]
        (reduce (fn [[args ctx] arg]
                  (let [subbed (maybe-subst-constant arg (:env ctx))
                        [new-arg new-ctx] (propagate-node subbed ctx)]
                    [(conj args new-arg) new-ctx]))
                [[] context]
                (n/call-args node))]
    [(n/make-call new-fn new-args (n/attrs node) (n/node-meta node))
     ctx1]))

;; ★ 核心：:alength 节点直接替换为已知长度 ──
(defmethod propagate-node :alength [node context]
  (let [target (n/alength-target node)
        len (when (n/variable-node? target)
              (env-get-array-len (:env context) (n/var-name target)))]
    (if len
      ;; 找到已知长度，直接替换为字面量
      [(n/make-literal len nil nil) context]
      ;; 未找到，保留原节点并递归处理目标
      (let [[new-target ctx1] (propagate-node (maybe-subst-constant target (:env context)) context)]
        [(n/make-alength new-target (n/node-meta node) ) ctx1]))))

(defmethod propagate-node :lambda [node context]
  (let [param-names (set (map n/var-name (n/lambda-params node)))
        inner-env (env-remove-vars (:env context) param-names)
        ctx-inner (assoc context :env inner-env)
        [new-body ctx2] (propagate-node (n/lambda-body node) ctx-inner)]
    [(n/make-lambda (n/lambda-params node) new-body
                    (n/lambda-captures node) (n/lambda-fn-name node)
                    (n/attrs node) (n/node-meta node))
     ctx2]))


(defmethod propagate-node :default [node context]
  (p/reduce-children node
                     (fn [child ctx]
                       (let [subbed (maybe-subst-constant child (:env ctx))
                             [new-child new-ctx] (propagate-node subbed ctx)]
                         [new-child new-ctx]))
                     context))

;; ── 上下文构造 ──
(defn make-context [compile-ctx frontend backend]
  {:ctx compile-ctx, :frontend frontend, :backend backend, :env (make-empty-env)})

;; ── 入口 ──
(defn propagate [ir2-roots context]
  (let [[new-roots final-ctx]
        (reduce (fn [[roots ctx] root]
                  (let [[new-root new-ctx] (propagate-node root ctx)]
                    [(conj roots new-root) new-ctx]))
                [[] context]
                ir2-roots)]
    {:roots new-roots :context final-ctx}))