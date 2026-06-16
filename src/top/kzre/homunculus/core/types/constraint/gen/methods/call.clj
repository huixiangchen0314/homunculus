(ns top.kzre.homunculus.core.types.constraint.gen.methods.call
  "约束生成：:call 节点。直接从 IFrontendInfo 协议获取内置函数/重载列表，
   不再依赖前序 Pass 写入的 :builtin-fn。"
  (:require
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.constraint.constraint :as c]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :call [node context]
  (let [;; 1. 推导函数表达式
        [fn-tv fn-node fn-constraints] (gen/cg-node-raw (n/call-fn node) context)

        ;; 2. 尝试从协议获取内置函数/重载
        fn-name (when (= :variable (ir2p/kind fn-node))
                  (n/var-name fn-node))
        builtin (when fn-name
                  (get (tp/builtin-functions (:frontend context)) fn-name))

        ;; 3. 确定最终参与约束生成的候选类型
        candidates (cond
                     (satisfies? tp/IType builtin) builtin   ;; 单个类型
                     (sequential? builtin)         builtin   ;; 重载列表
                     :else                         fn-tv)    ;; 回退到推导结果

        ;; 4. 推导实参
        args (n/call-args node)
        results (map #(gen/cg-node-raw % context) args)
        arg-tys (mapv first results)
        arg-nodes (mapv second results)
        arg-constraints (mapcat #(nth % 2) results)
        ret-tv (gen/fresh-tvar)]

    (if (and (sequential? candidates)
             (seq candidates)
             (not (scheme/tscheme? (first candidates))))
      ;; 重载列表 → 产生 COverload 约束
      [ret-tv
       (ty/set-type! node ret-tv)
       (concat (list (c/make-coverload (vec candidates) arg-tys ret-tv node))
               fn-constraints
               arg-constraints)]
      ;; 单类型 → 产生 CEqual 约束
      (let [desired (reduce (fn [ret arg] (ty/make-tfun arg ret)) ret-tv (reverse arg-tys))
            new-node (n/make-call fn-node (vec arg-nodes)
                                  (n/attrs node) (n/node-meta node) (n/parent node))]
        [ret-tv
         (ty/set-type! new-node ret-tv)
         (concat (list (c/make-cequal candidates desired))
                 fn-constraints
                 arg-constraints)]))))