(ns top.kzre.homunculus.core.types.constraint.gen.methods.call
  "约束生成：:call 节点。直接从 IFrontendInfo 协议获取内置函数/重载列表，
   不再依赖前序 Pass 写入的 :builtin-fn。"
  (:require
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.model :as t]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.constraint.model :as cm]
    [top.kzre.homunculus.core.ir2.model :as m]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :call [node context]
  (let [;; 1. 推导函数表达式
        [fn-tv fn-node fn-constraints] (gen/cg-node-raw (:fn node) context)

        ;; 2. 尝试从协议获取内置函数/重载
        ;;    只有函数位置是变量时，才可能对应内置函数名
        fn-name (when (= :variable (ir2p/kind fn-node))
                  (:name fn-node))
        builtin (when fn-name
                  (get (tp/builtin-functions (:frontend context)) fn-name))

        ;; 3. 确定最终参与约束生成的候选类型
        ;;    builtin 可能的情况：
        ;;      - nil                 → 使用推导出的 fn-tv (可能是一个 TVar)
        ;;      - IType 实例          → 直接使用该类型
        ;;      - 向量/列表/集合       → 重载候选列表（每个元素是 IType）
        candidates (cond
                     (satisfies? tp/IType builtin) builtin   ;; 单个类型
                     (coll? builtin)              builtin   ;; 重载列表 (如 [TFun, TFun, ...])
                     :else                        fn-tv)    ;; 回退到推导结果

        ;; 4. 推导实参
        args (:args node)
        results (map #(gen/cg-node-raw % context) args)
        arg-tys (mapv first results)
        arg-nodes (mapv second results)
        arg-constraints (mapcat #(nth % 2) results)

        ret-tv (gen/fresh-tvar)]

    ;; 5. 根据候选形式生成对应的约束
    (if (and (sequential? candidates)           ;; 是一个集合
             (seq candidates)             ;; 非空
             (not (scheme/tscheme? (first candidates))))  ;; 第一个不是 TScheme
      ;; 重载列表 → 产生 COverload 约束
      [ret-tv
       (ty/set-type! node ret-tv)
       (concat (list (cm/->COverload (vec candidates) arg-tys ret-tv node))
               fn-constraints
               arg-constraints)]
      ;; 单类型（具体函数类型）或 TVar → 产生 CEqual 约束
      (let [desired (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tv (reverse arg-tys))
            new-node (m/->CallNode fn-node (vec arg-nodes)
                                   (:attrs node) (:meta node) (:parent node))]
        [ret-tv
         (ty/set-type! new-node ret-tv)
         (concat (list (cm/->CEqual candidates desired))
                 fn-constraints
                 arg-constraints)]))))