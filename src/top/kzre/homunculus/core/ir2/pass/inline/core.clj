(ns top.kzre.homunculus.core.ir2.pass.inline.core
  "基于标记的内联 Pass：处理用户标记的 :inline 函数以及多态函数的内联。
   使用 reduce-children 统一递归，无需手写遍历。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.transform.inline :as transform-inline]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]
    [top.kzre.homunculus.core.ir2.transform.rename.rename :as rename]
    [top.kzre.homunculus.core.ir2.pass.inline.env :as p]))

;; ── 内联辅助 ────────────────────────────
(defn- clear-node [node]
  (-> node ty/clear-type rename/rename-node))

(defn- try-inline-call
  "如果 fn-node 命中可内联 lambda，展开为 body；否则原样返回调用节点。"
  [fn-node args node env]
  (let [fn-name (when (= (ir2/kind fn-node) :variable)
                  (n/var-name fn-node))
        lam     (when fn-name (p/find-inlineable env fn-name))]
    (if lam
      (clear-node (transform-inline/inline-args
                    (n/lambda-params lam)
                    args
                    (n/lambda-body lam)))
      (n/make-call fn-node args (n/attrs node) (n/node-meta node)))))

(defn- register-if-inlineable
  "如果节点是带 :inline / :polymorphic 标记的 define，注册进 env。"
  [env node]
  (if (and (= (ir2/kind node) :define)
           (n/define-val node)
           (= (ir2/kind (n/define-val node)) :lambda))
    (let [attrs (n/attrs node)]
      (if (or (true? (:inline attrs))
              (true? (:polymorphic attrs)))
        (p/register-inline-def env (n/define-name node) (n/define-val node))
        env))
    env))

;; ── 核心遍历 ─────────────────────────────
(declare walk)

(defn- inline-node
  [node env]
  (case (ir2/kind node)
    :call
    (let [fn-node (n/call-fn node)
          args    (n/call-args node)
          inlined (try-inline-call fn-node args node env)]
      ;; 对替换后的节点继续遍历，以处理可能出现的嵌套内联
      (walk inlined env))

    :define
    (let [val               (n/define-val node)
          [new-val new-env] (if val (inline-node val env) [nil env])
          new-node          (n/make-define (n/define-name node)
                                           new-val
                                           (n/define-docstring node)
                                           (n/attrs node)
                                           (n/node-meta node))]
      [new-node (register-if-inlineable new-env new-node)])

    (walk node env)))

(defn walk
  [node env]
  (ir2/reduce-children node inline-node env))

;; ── 入口 ──
(defn inline
  "对 IR2 根节点列表执行内联。"
  [ir2-roots env]
  (first
    (reduce (fn [[roots e] root]
              (let [[new-root new-env] (inline-node root e)]
                [(conj roots new-root) new-env]))
            [[] env]
            ir2-roots)))