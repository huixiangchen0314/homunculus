(ns top.kzre.homunculus.core.types.typed.core
  "IR2 HM 类型推导 pass。基于 IR2 协议树和共享类型系统。"
  (:require [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.types.typed.env :as e]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(def ^:dynamic *tv-id (atom 0))
(defn fresh-tvar [] (t/->TVar (swap! *tv-id inc)))

;; 多方法分派：基于 IR2 节点的 kind
(defmulti infer
          (fn [node context]
            (ir2p/kind node)))

;; ── 顶层入口 ──
(defn type-check
  [ir2-roots & {:keys [frontend backend builtins]}]
  (binding [*tv-id (atom 0)]
    (let [context {:frontend frontend
                   :backend backend
                   :tv-id *tv-id}
          env (or builtins {})
          ;; 按顺序处理 IR2 根节点序列，并维护环境（处理 define 时扩展）
          infer-top (fn infer-top [env exprs]
                      (when-let [root (first exprs)]
                        (let [[_ new-root] (infer root (assoc context :env env))
                              new-env (if (= (ir2p/kind new-root) :define)
                                        (e/extend-env env (:name new-root)
                                                      (-> new-root ir2p/attrs :type))
                                        env)]
                          (cons new-root (lazy-seq (infer-top new-env (rest exprs)))))))]
      (doall (infer-top env ir2-roots)))))