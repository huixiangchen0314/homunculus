(ns top.kzre.homunculus.core.types.alpha-rename
  "Alpha 重命名：为所有局部变量生成唯一名称，避免变量捕获。
   使用 reduce-children 统一遍历，仅对顺序作用域手动处理。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.utils :as u]
            [top.kzre.homunculus.core.ir2.protocol :as p]))

(defrecord Env [table])
(defn make-env [] (->Env {}))

(defn- extend-env [env old new] (update env :table assoc old new))
(defn- extend-all [env pairs] (update env :table into pairs))

(declare walk)

(defn- rename-fn [node env]
  (case (p/kind node)
    :variable
    (if-let [new-name (get (:table env) (n/var-name node))]
      [(n/make-variable new-name (n/attrs node) (n/node-meta node)) env]
      [node env])

    :let
    (let [bindings (n/let-bindings node)
          [new-bindings env']
          (reduce (fn [[bnds e] [var val]]
                    (let [[new-val e1] (rename-fn val e)
                          old-name (n/var-name var)
                          new-name (u/fresh-name old-name)
                          e2 (extend-env e1 old-name new-name)
                          new-var (n/make-variable new-name (n/attrs var) (n/node-meta var))]
                      [(conj bnds [new-var new-val]) e2]))
                  [[] env] bindings)
          [new-body env''] (rename-fn (n/let-body node) env')]
      [(n/make-let new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node)) env''])

    :loop
    (let [bindings (n/loop-bindings node)
          [new-bindings env']
          (reduce (fn [[bnds e] [var val]]
                    (let [[new-val e1] (rename-fn val e)
                          old-name (n/var-name var)
                          new-name (u/fresh-name old-name)
                          e2 (extend-env e1 old-name new-name)
                          new-var (n/make-variable new-name (n/attrs var) (n/node-meta var))]
                      [(conj bnds [new-var new-val]) e2]))
                  [[] env] bindings)
          [new-body env''] (rename-fn (n/loop-body node) env')]
      [(n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node)) env''])

    :lambda
    (let [params (n/lambda-params node)
          old-names (map n/var-name params)
          new-names (map u/fresh-name old-names)
          new-env (extend-all env (map vector old-names new-names))]
      [node new-env])   ;; 返回原节点，reduce-children 会递归子节点

    :catch
    (let [old-sym (n/catch-sym node)
          old-name (n/var-name old-sym)
          new-name (u/fresh-name old-name)
          new-env (extend-env env old-name new-name)]
      [node new-env])   ;; 同上

    ;; 其他容器节点：直接递归子节点
    (walk node env)))

(defn walk [node env]
  (p/reduce-children node rename-fn env))

(defn rename-nodes
  "对 IR2 根节点列表进行 alpha 重命名，环境在节点间顺序传递。"
  [nodes]
  (let [env (make-env)
        [new-nodes _] (reduce (fn [[ns e] node]
                                (let [[nn ne] (rename-fn node e)]
                                  [(conj ns nn) ne]))
                              [[] env] nodes)]
    new-nodes))
