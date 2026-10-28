(ns top.kzre.homunculus.core.ir2.pass.alias
  "别名应用 Pass：遍历 IR2 树，对每个 :variable 节点通过 resolve-symbol-entry 查询。
   如果是 :alias 条目，替换为 target；否则保持原样。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.ast :as p]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.core.symbol :as sym]))

(declare walk)

(defn- resolve-alias-target
  "如果 sym 对应一个 :alias 条目，返回其 target；否则返回 nil。"
  [ctx sym]
  (when-let [entry (ip/resolve-symbol-entry ctx sym)]
    (when-let [alias-entry (sym/entry->alias entry)]
      (sym/alias-target alias-entry))))

(defn- alias-fn
  "处理单个节点：variable 替换 alias，其他节点原样。"
  [node ctx]
  (if (= :variable (p/kind node))
    (let [var-name (n/var-name node)]
      [(if-let [target (resolve-alias-target ctx var-name)]
         (n/make-variable target (n/attrs node) (n/node-meta node))
         node)
       ctx])
    (walk node ctx)))

(defn walk
  [node ctx]
  (p/reduce-children node alias-fn ctx))

(defn alias-nodes
  [ir2-roots compile-ctx]
  (mapv (fn [root]
          (first (alias-fn root compile-ctx)))
        ir2-roots))