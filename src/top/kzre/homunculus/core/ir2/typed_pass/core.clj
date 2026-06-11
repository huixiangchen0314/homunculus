(ns top.kzre.homunculus.core.ir2.typed-pass.core
  "IR2 类型推导 pass 入口。定义 multimethod infer 和顶层 type-check。
   不加载任何方法文件，方法由 typed-pass.methods 注册。"
  (:require [top.kzre.homunculus.core.ir2.typed-pass.types :as t]
            [top.kzre.homunculus.core.ir2.typed-pass.env :as e]))

(defmulti infer (fn [node env] (:kind node)))

(defn type-check [ir2-toplevel & {:keys [builtins]}]
  (binding [t/*tv-id (atom 0)]
    (let [env (or builtins {})
          infer-top (fn infer-top [env exprs]
                      (when-let [e (first exprs)]
                        (let [[_ node] (infer e env)
                              new-env (if (= (:kind node) :define)
                                        (e/extend-env env (:name node)
                                                      (get-in node [:attrs :type]))
                                        env)]
                          (cons node (lazy-seq (infer-top new-env (rest exprs)))))))]
      (doall (infer-top env ir2-toplevel)))))