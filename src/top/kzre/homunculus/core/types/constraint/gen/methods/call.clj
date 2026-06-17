(ns top.kzre.homunculus.core.types.constraint.gen.methods.call
  "约束生成：:call 节点。直接从符号表获取函数重载类型。"
  (:require
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.constraint.constraint :as c]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.internal.symbol :as sym]))

(defn- arity->tfun [arity]
  (reduce (fn [ret param] (ty/make-tfun (:type param) ret))
          (some-> (:ret arity) :type)
          (reverse (:params arity))))

(defmethod gen/cg-node-raw :call [node context]
  (let [[fn-tv fn-node fn-constraints] (gen/cg-node-raw (n/call-fn node) context)
        fn-name (when (= (n/kind fn-node) :variable)
                  (n/var-name fn-node))
        entry (when fn-name
                (sym/lookup-in-tables fn-name (:symbol-table context)))
        candidates (when (sym/function-symbol? entry)
                     (mapv (fn [arity]
                             (reduce (fn [ret param] (ty/make-tfun (:type param) ret))
                                     (some-> (:ret arity) :type)
                                     (reverse (:params arity))))
                           (sym/list-arities entry)))
        args (n/call-args node)
        results (map #(gen/cg-node-raw % context) args)
        arg-tys (mapv first results)
        arg-nodes (mapv second results)
        arg-constraints (mapcat #(nth % 2) results)
        ret-tv (gen/fresh-tvar)]
    (if (seq candidates)
      ;; 始终生成 COverload 约束
      [ret-tv
       (ty/set-type! node ret-tv)
       (concat (list (c/make-coverload candidates arg-tys ret-tv node))
               fn-constraints arg-constraints)]
      (let [tv (gen/fresh-tvar)]
        [tv (ty/set-type! node tv) nil]))))