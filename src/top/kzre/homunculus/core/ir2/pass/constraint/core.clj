(ns top.kzre.homunculus.core.ir2.pass.constraint.core
  "约束系统的编排入口：构造上下文、运行约束生成与求解。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.constraint.env :as env]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as gen.env]
    [top.kzre.homunculus.core.ir2.pass.constraint.protocol :as proto]
    [top.kzre.homunculus.core.ir2.pass.constraint.unify :as unify]))

(defn solve
  [ir2-roots compile-ctx]
  (let [{:keys [nodes constraints]}
        (gen/gen ir2-roots (gen.env/make-env compile-ctx ))]
    (loop [constrs constraints
           subs-map {}
           env (env/make-env compile-ctx)]
      (let [[remaining subst-map' env']
            (reduce
              (fn [[cs substs e] c]
                (let [s (proto/solve-constraint c substs e)
                      c' (proto/substitute-constraint c s)]
                  (if (proto/solved? c')
                    [cs s e]
                    [(conj cs c') s e])))
              [[] subs-map env]
              constrs)]
        (if (or (empty? remaining)
                 (= subs-map subst-map'))
          (unify/subst-nodes nodes subst-map')
          (recur remaining subst-map' env'))))))