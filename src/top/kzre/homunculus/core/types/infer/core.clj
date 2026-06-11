;; top.kzre.homunculus.core.types.infer.core.clj
(ns top.kzre.homunculus.core.types.infer.core
  "轻量级局部类型推导 pass（前向传播）。"
  (:require [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmulti local-infer
          (fn [node context] (ir2p/kind node)))

(defn success [ty node] [ty node])   ;; 改为公有
(defn nothing [node] [nil node])     ;; 改为公有

(defn run
  [ir2-roots & {:keys [frontend]}]
  (let [context {:frontend frontend :env {}}
        infer-seq (fn infer-seq [env nodes]
                    (when-let [root (first nodes)]
                      (let [[ty new-root] (local-infer root (assoc context :env env))
                            new-env (if (and ty (= (ir2p/kind new-root) :define))
                                      (e/extend-env env (:name new-root) ty)
                                      env)]
                        (cons new-root (lazy-seq (infer-seq new-env (rest nodes)))))))]
    (doall (infer-seq {} ir2-roots))))