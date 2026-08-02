(ns top.kzre.homunculus.core.types.alias
  "别名应用 Pass：遍历 IR2 树，将符号表中的别名替换为目标符号。
   使用 reduce-children 统一递归，修复别名方向错误。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.protocol :as p]
    [top.kzre.homunculus.core.types.protocol :as types]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.internal.symbol :as sym]))

(defn- build-alias-map [symbol-table]
  (into {}
        (keep (fn [[_ entry]]
                (when (sym/alias-symbol? entry)
                  (let [alias-name (symbol (name (:sym entry)))   ;; 短名
                        target-sym (sym/alias-target entry)]     ;; 目标符号（保持原样）
                    [alias-name target-sym]))))
        symbol-table))

(declare walk)

(defn- alias-fn
  "处理单个节点：变量替换，其他节点原样返回。"
  [node alias-map]
  (if (= :variable (p/kind node))
    (let [var-name (n/var-name node)]
      (if (and (not (namespace var-name))
               (contains? alias-map var-name))
        [(n/make-variable (get alias-map var-name)
                          (n/attrs node) (n/node-meta node))
         alias-map]
        [node alias-map]))
    (walk node alias-map)))

(defn walk
  "docstring"
  [node alias-map]
  (p/reduce-children node alias-fn alias-map))

(defn alias-nodes
  [ir2-roots context frontend]
  (let [builtin-table (types/builtin-symbols frontend)
        user-table    (ip/symbol-table context)
        combined-table (merge builtin-table user-table)
        alias-map (build-alias-map combined-table)]
    (mapv (fn [root]
            (first (alias-fn root alias-map)))
          ir2-roots)))