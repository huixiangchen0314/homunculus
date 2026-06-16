(ns top.kzre.homunculus.core.types.module.resolve-ns
  "命名空间解析：提取 ns 声明，注册依赖，替换别名。"
  (:require
    [clojure.walk :as w]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.internal.protocol :as ip]))

(defn- qualify-aliases
  "将 IR2 树中所有通过别名引用的变量名替换为完全限定名。
   aliases 是 {alias-sym -> full-ns-sym} 的映射。
   例如：:require [top.kzre.homunculus.examples.hlsl_lambert.core :as lambert]
   之后代码中的 lambert/foo 会被替换为 top.kzre.homunculus.examples.hlsl_lambert.core/foo。"
  [ir2-roots aliases]
  (w/prewalk
    (fn [node]
      (if (and (satisfies? ir2p/INode node)
               (= (ir2p/kind node) :variable))
        (let [var-name (:name node)]
          (if (and (symbol? var-name) (namespace var-name))
            (if-let [full-ns (get aliases (symbol (namespace var-name)))]
              (assoc node :name (symbol (str full-ns) (name var-name)))
              node)
            node))
        node))
    ir2-roots))

(defn resolve-ns
  "处理命名空间声明：
   - 提取 :ns 节点
   - 向编译上下文注册依赖
   - 替换别名引用
   返回处理后的 IR2 节点列表。"
  [ir2-roots context]
  (let [;; 分离 ns 节点与其余节点
        ns-nodes      (filter #(= (ir2p/kind %) :ns) ir2-roots)
        non-ns-roots  (remove #(= (ir2p/kind %) :ns) ir2-roots)

        ;; 收集所有 require 子句中的依赖
        dep-syms (mapcat
                   (fn [ns-node]
                     (let [refs (:references ns-node)] ; 预期是 [(:require ...) ...]
                       (mapcat
                         (fn [ref]
                           (when (and (seq? ref) (= :require (first ref)))
                             (keep (fn [spec]
                                     (if (symbol? spec)
                                       spec
                                       ;; (ns-name :as alias) → ns-name
                                       (first spec)))
                                   (rest ref))))
                         refs)))
                   ns-nodes)

        ;; 构建别名映射 {alias -> full-ns}
        aliases (into {}
                      (mapcat
                        (fn [ns-node]
                          (let [refs (:references ns-node)]
                            (mapcat
                              (fn [ref]
                                (when (and (seq? ref) (= :require (first ref)))
                                  (keep (fn [spec]
                                          (when (and (sequential? spec) (= 3 (count spec)) (= :as (second spec)))
                                            [(nth spec 2) (first spec)]))
                                        (rest ref))))
                              refs)))
                        ns-nodes))]

    ;; 1) 向编译上下文注册依赖（上下文会处理递归编译和类型缓存）
    (ip/register-deps context dep-syms)

    ;; 2) 用别名替换非 ns 节点中的变量引用
    (let [qualified-non-ns (qualify-aliases non-ns-roots aliases)]
      ;; 将 ns 节点放在最前面（通常只有一个 ns），其余节点在后
      (into (vec ns-nodes) qualified-non-ns))))