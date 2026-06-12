(ns top.kzre.homunculus.core.types.typed.core
  "IR2 HM 类型推导 pass。基于 IR2 协议树和共享类型系统。"
  (:require [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(def ^:dynamic *tv-id (atom 0))
(defn apply-subst-to-env [env subst]
  (into {} (map (fn [[k v]] [k (u/substitute v subst)]) env)))

(defn fresh-tvar [] (t/->TVar (swap! *tv-id inc)))

;; 多方法分派：基于 IR2 节点的 kind
(defmulti infer
          "返回 [type, updated-node, substitution] 三元组。"
          (fn [node context]
            (ir2p/kind node)))

;; ── 顶层入口 ──
(defn type-check [ir2-roots & {:keys [frontend backend builtins]}]
  (binding [*tv-id (atom 0)]
    (let [context {:frontend frontend :backend backend :tv-id *tv-id}
          env (or builtins {})
          infer-top (fn infer-top [env nodes]
                      (when-let [root (first nodes)]
                        (let [[ty new-root s] (infer root (assoc context :env env))
                              ;; 应用替换到环境
                              env' (apply-subst-to-env env s)
                              ;; 如果是 define，扩展环境
                              new-env (if (= (ir2p/kind new-root) :define)
                                        (e/extend-env env' (:name new-root) ty)
                                        env')]
                          (cons new-root (lazy-seq (infer-top new-env (rest nodes)))))))]
      (doall (infer-top env ir2-roots)))))