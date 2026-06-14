(ns top.kzre.homunculus.core.types.loading.pass
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.internal.protocol :as ip]))

(defn- qualify-aliases
  "将 IR2 树中所有通过 :as 别名引用的变量替换为完全限定名。
   aliases 是 {alias-sym -> full-ns-sym} 的映射。"
  [ir2-roots aliases]
  ;; 实现：遍历 IR2 树，将 VariableNode 的 name 如果匹配别名则替换为 full/name
  ;; 此处简化，实际需递归处理所有节点
  (clojure.walk/postwalk
    (fn [node]
      (if (and (satisfies? ir2p/INode node)
               (= (ir2p/kind node) :variable))
        (if-let [full-ns (get aliases (symbol (namespace (:name node))))]
          (assoc node :name (symbol (str full-ns "/" (name (:name node)))))
          node)
        node))
    ir2-roots))

(defn namespace-pass [ir2-roots context]
  (let [ns-nodes (filter #(= (ir2p/kind %) :ns) ir2-roots)
        other-roots (remove #(= (ir2p/kind %) :ns) ir2-roots)]
    (doseq [ns-node ns-nodes]
      (let [require-clauses (->> (:references ns-node)
                                 (filter #(= :require (first %))))
            ;; 收集所有依赖命名空间符号
            dep-syms (mapcat (fn [clause]
                               (map (fn [spec]
                                      (if (symbol? spec) spec (first spec)))
                                    (rest clause)))
                             require-clauses)
            ;; 提取别名映射
            aliases (into {}
                          (mapcat (fn [clause]
                                    (keep (fn [spec]
                                            (when (and (seq? spec) (:as (apply hash-map (rest spec))))
                                              [(second spec) (first spec)]))
                                          (rest clause)))
                                  require-clauses))]
        ;; 1. 通知上下文加载依赖（上下文负责递归编译并缓存类型）
        (ip/register-deps context dep-syms)
        ;; 2. 将别名引用替换为完全限定名
        (let [qualified-others (qualify-aliases other-roots aliases)]
          ;; 返回处理后的 IR2，保留 ns 节点供后端生成导入语句
          (concat qualified-others ns-nodes))))))