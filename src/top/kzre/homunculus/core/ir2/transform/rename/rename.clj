(ns top.kzre.homunculus.core.ir2.transform.rename.rename
  "Alpha 重命名：为所有局部变量生成唯一名称，避免变量捕获。

   使用 reduce-children 统一遍历，仅对顺序作用域手动处理。

   属通用变换——语义保持，只改变变量名。
   被多个 pass 依赖（inline / lambda-elim / ...）。"
  (:require
    [top.kzre.homunculus.core.ir2.ast :as p]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.utils :as u]
    [top.kzre.homunculus.core.ir2.transform.rename.env :as env]))

(declare rename-fn walk)

;; ═══════════════════════════════════════════════
;; 顺序作用域：let / loop
;; ═══════════════════════════════════════════════

(defn- rename-bindings
  "重命名一组绑定。返回 [新绑定列表 新环境]。
   每个绑定的 init 表达式在当前环境下重命名，
   然后扩展环境——后续绑定能看到前面的重命名。"
  [bindings env*]
  (reduce
    (fn [[bnds e] b]
      (let [var          (:var b)
            val          (:val b)
            [new-val e1] (rename-fn val e)
            old-name     (:name var)
            new-name     (u/fresh-name old-name)
            e2           (env/bind e1 old-name new-name)
            new-var      (assoc var :name new-name)
            new-b        (assoc b :var new-var :val new-val)]
        [(conj bnds new-b) e2]))
    [[] env*]
    bindings))

(defn- rename-scoped
  "处理顺序作用域节点（let / loop）。
   提取绑定、重命名绑定、重命名 body、用 make-fn 重建节点。"
  [node env* bindings body-kw make-fn]
  (let [[new-bindings env'] (rename-bindings bindings env*)
        [new-body env'']     (rename-fn (get node body-kw) env')]
    [(make-fn node new-bindings new-body) env'']))

;; ═══════════════════════════════════════════════
;; 分派
;; ═══════════════════════════════════════════════

(declare rename-fn)

(defn- rename-fn
  [node env*]
  (case (p/kind node)
    :variable
    (if-let [new-name (env/resolve-subst-name env* (n/var-name node))]
      [(n/make-variable new-name (n/attrs node) (n/node-meta node)) env*]
      [node env*])

    :let
    (rename-scoped node env*
                   (n/let-bindings node)
                   :body
                   (fn [n bs b] (n/make-let bs b
                                            (n/attrs n) (n/node-meta n))))

    :loop
    (rename-scoped node env*
                   (n/loop-bindings node)
                   :body
                   (fn [n bs b] (n/make-loop bs b
                                             (n/attrs n) (n/node-meta n))))

    :lambda
    ;; 扩展环境——params 与 body 由 reduce-children 递归处理
    (let [params    (n/lambda-params node)
          old-names (map n/var-name params)
          new-names (map u/fresh-name old-names)
          new-env   (env/bind-all env* (map vector old-names new-names))]
      [node new-env])

    :catch
    ;; 扩展环境——catch body 由 reduce-children 递归处理
    (let [old-sym  (n/catch-sym node)
          old-name (n/var-name old-sym)
          new-name (u/fresh-name old-name)
          new-env  (env/bind env* old-name new-name)]
      [node new-env])

    ;; 其他容器节点：递归子节点
    (walk node env*)))

(defn- walk
  [node env*]
  (p/reduce-children node rename-fn env*))

;; ═══════════════════════════════════════════════
;; 对外入口
;; ═══════════════════════════════════════════════

(defn rename-node
  "对单个 IR2 根节点做 alpha 重命名。"
  [node]
  (first (rename-fn node (env/make-env))))

(defn rename
  "对 IR2 根节点列表做 alpha 重命名。
   环境在节点间顺序传递——前面节点引入的绑定对后续节点可见。"
  [nodes]
  (first
    (reduce (fn [[ns e] node]
              (let [[nn ne] (rename-fn node e)]
                [(conj ns nn) ne]))
            [[] (env/make-env)]
            nodes)))