(ns top.kzre.homunculus.core.ir1.reference
  "ns 引用的处理：加载宏 ns、注册依赖、展开 :refer :all。"
  (:require
    [top.kzre.homunculus.core.ir1.ns-form :as ns-form]
    [top.kzre.homunculus.internal.protocol :as ip]))

(defn resolve-alias
  [nsf sym]
  (or (first (ns-form/reference-alias-in-ns nsf :require-macros sym))
      (first (ns-form/reference-alias-in-ns nsf :require sym))))

;; ── 宏加载 ──────────────────────────────
(defn- inject-global-macros [ctx nsf]
  (let [globals  (seq (ip/macro-namespaces ctx))
        declared (set (mapv first (ns-form/reference-entry nsf :require-macros)))
        new-ns   (remove declared globals)]
    (if (seq new-ns)
      (apply ns-form/prepend-reference nsf :require-macros
             (mapv (fn [ns*] [ns* :refer :all]) new-ns))
      nsf)))

(defn- load-all-macros! [ctx nsf]
  (doseq [ns* (mapv first (ns-form/reference-entry nsf :require-macros))]
    (ip/load-macro-ns! ctx ns*)))

;; ── :refer :all 展开 ─────────────────────
(defn- refer-all? [libspec]
  (when (vector? libspec)
    (let [opts (apply hash-map (rest libspec))]
      (or (= :all (:refer opts)) (= 'all (:refer opts))))))

(defn- rewrite-refer [libspec exports]
  (let [[ns-sym & kv] libspec
        opts          (apply hash-map kv)
        others        (dissoc opts :refer)]
    (into [ns-sym] (concat (mapcat identity others)
                           [:refer (vec exports)]))))

(defn- register-deps!
  "触发依赖编译（副作用）。返回 nsf 本身，方便链式。"
  [ctx nsf]
  (let [macro-ns (set (mapv first (ns-form/reference-entry nsf :require-macros)))
        req-syms (vec (->> (ns-form/reference-entry nsf :require)
                           (keep first)
                           (remove macro-ns)))]
    (ip/register-deps ctx req-syms)
    nsf))

(defn- expand-refer-all [ctx nsf]
  (let [expand-one
        (fn [libspec]
          (if (refer-all? libspec)
            (rewrite-refer libspec (ip/ns-exported-syms ctx (first libspec)))
            libspec))]
    (update nsf :references update :require #(mapv expand-one %))))

;; ── 主入口 ──────────────────────────────
(defn resolve-ns! [ctx nsf]
  (if ctx
    (do
      (load-all-macros! ctx nsf)      ; 加载 :require-macros 里的宏
      (->> nsf
           (register-deps! ctx)       ; 编译 :require 里的依赖
           (expand-refer-all ctx)))   ; 展开 :refer :all
    nsf))