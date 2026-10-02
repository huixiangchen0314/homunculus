(ns top.kzre.homunculus.core.ir2.pass.ho-elim.core
  "高阶函数内联 Pass：标记并内联高阶函数。
   使用 reduce-children + walk 模式，elim-node* 绝不递归子节点。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.transform.inline :as transform-inline]
    [top.kzre.homunculus.core.ir2.pass.mark-ho :as mark-ho]
    [top.kzre.homunculus.core.ir2.pass.ho-elim.env :as env]))

;; ── 内部单步 ─────────────────────────────
(declare walk)

(defn- elim-node*
  [node env]
  (case (ir2/kind node)
    ;; define：被标记为高阶的 lambda 注册进环境
    :define
    (let [name (n/define-name node)
          val  (n/define-val node)
          ho?  (-> node n/attrs :ho?)]
      (if (and val ho? (n/lambda-node? val))
        (walk node (env/track-ho env name val))
        (walk node env)))

    ;; call：尝试内联高阶调用
    :call
    (let [fn-node (n/call-fn node)
          args    (n/call-args node)]
      (if (and (n/variable-node? fn-node)
               (< (env/depth env) (env/max-depth env)))
        (let [fn-name (n/var-name fn-node)
              lam     (env/lookup-ho env fn-name)]
          (if lam
            (let [inlined (transform-inline/inline-args
                            (n/lambda-params lam)
                            args
                            (n/lambda-body lam))
                  env'    (-> env
                              (env/track-ho fn-name lam)
                              (env/with-depth (inc (env/depth env))))]
              (walk inlined env'))
            (walk node env)))
        (walk node env)))

    (walk node env)))

(defn- walk
  [node env]
  (ir2/reduce-children node elim-node* env))

;; ── 对外入口 ─────────────────────────────
(defn elim-node
  "对单个节点做高阶内联。"
  [node env]
  (elim-node* node env))

(defn elim
  "对 IR2 根节点列表做高阶内联。"
  [ir2-roots env]
  (let [analyzed-roots (mark-ho/mark ir2-roots)]
    (first
      (reduce (fn [[roots e] root]
                (let [[new-root new-env] (elim-node* root e)]
                  [(conj roots new-root) new-env]))
              [[] env]
              analyzed-roots))))