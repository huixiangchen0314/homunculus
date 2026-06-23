(ns top.kzre.homunculus.core.types.module.resolve-ns
  "命名空间解析：提取 ns 声明，注册依赖，替换别名。
   忽略仅用于宏展开的命名空间。"
  (:require
    [clojure.walk :as w]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.protocol :as types]
    [top.kzre.homunculus.core.types.namespace :as ns-info]
    [top.kzre.homunculus.internal.protocol :as ip]))

(defn- qualify-aliases
  ;; 不变，同前
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
   - 根据 frontend 过滤宏命名空间后注册依赖
   - 替换别名引用
   返回处理后的 IR2 节点列表。"
  [ir2-roots context frontend]
  (let [ns-nodes      (filter #(= (ir2p/kind %) :ns) ir2-roots)
        non-ns-roots  (remove #(= (ir2p/kind %) :ns) ir2-roots)
        macro-ns      (when frontend (types/macro-namespaces frontend)) ; 可能为空集合
        macro-ns      (or macro-ns #{})
        ;; 收集 require 依赖符号
        dep-syms (mapcat ns-info/ns-dependency-syms ns-nodes)
        ;; 过滤掉宏命名空间
        dep-syms (remove macro-ns dep-syms)

        ;; 构建别名映射 {alias -> full-ns}
        aliases (into {}
                      (mapcat
                        (fn [ns-node]
                          (let [refs (:references ns-node)]
                            (mapcat
                              (fn [ref]
                                (when (and (seq? ref) (= :require (first ref)))
                                  (keep (fn [spec]
                                          (when (and (sequential? spec)
                                                     (= (count spec) 3)
                                                     (= :as (second spec)))
                                            [(nth spec 2) (first spec)]))
                                        (rest ref))))
                              refs)))
                        ns-nodes))]

    ;; 1) 向编译上下文注册依赖（已排除宏命名空间）
    (ip/register-deps context dep-syms)

    ;; 2) 用别名替换非 ns 节点中的变量引用
    (let [qualified-non-ns (qualify-aliases non-ns-roots aliases)]
      (into (vec ns-nodes) qualified-non-ns))))