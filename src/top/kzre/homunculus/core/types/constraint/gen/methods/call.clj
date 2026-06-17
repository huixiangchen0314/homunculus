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
  (let [;; 1. 推导函数表达式
        [fn-tv fn-node fn-constraints] (gen/cg-node-raw (n/call-fn node) context)
        ;; 2. 获取函数名（仅变量）
        fn-name (when (= (n/kind fn-node) :variable)
                  (n/var-name fn-node))
        ;; 3. 从符号表获取函数条目
        entry (when fn-name
                (sym/lookup-in-tables fn-name (:symbol-table context)))
        ;; 4. 提取重载列表
        arities (when (sym/function-symbol? entry)
                  (sym/list-arities entry))
        ;; 5. 构造候选类型列表（每个重载作为一个候选 TFun）
        candidates (when arities
                     (mapv arity->tfun arities))
        ;; 6. 推导实参
        args (n/call-args node)
        results (map #(gen/cg-node-raw % context) args)
        arg-tys (mapv first results)
        arg-nodes (mapv second results)
        arg-constraints (mapcat #(nth % 2) results)
        ret-tv (gen/fresh-tvar)]

    (if (and candidates (seq candidates))
      (if (= (count candidates) 1)
        ;; 单重载 → CEqual
        (let [desired (reduce (fn [ret arg] (ty/make-tfun arg ret)) ret-tv (reverse arg-tys))
              new-node (n/make-call fn-node (vec arg-nodes)
                                    (n/attrs node) (n/node-meta node) (n/parent node))]
          [ret-tv
           (ty/set-type! new-node ret-tv)
           (concat (list (c/make-cequal (first candidates) desired))
                   fn-constraints arg-constraints)])
        ;; 多重载 → COverload
        [ret-tv
         (ty/set-type! node ret-tv)
         (concat (list (c/make-coverload candidates arg-tys ret-tv node))
                 fn-constraints arg-constraints)])
      ;; 符号表无此函数，分配 TVar
      (let [tv (gen/fresh-tvar)]
        [tv (ty/set-type! node tv) nil]))))