;; top.kzre.homunculus.core.types.infer.core.clj
(ns top.kzre.homunculus.core.types.infer.core
  "轻量级局部类型推导 pass（前向传播）。"
  (:require
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.core.ir2.protocol :as ir2p]
   [top.kzre.homunculus.core.types.env :as e]
   [top.kzre.homunculus.core.types.protocol :as tp]
   [top.kzre.homunculus.internal.protocol :as ip]
   [top.kzre.homunculus.internal.symbol :as sym]))

(defmulti local-infer
          "对节点树递归进行局部类型推导, 返回向量，其形式为 [type new-node]
          其中, type 为当前节点推断类型，如果未能推断，返回nil,
          new-node 为重建后的当前节点(包括子节点)."
          (fn [node _context] (ir2p/kind node)))

(defn frontend
  "辅助函数，从上下文获取前端协议对象"
  [contex] (:frontend contex))

(defn env
  "辅助函数，从上下文获取环境map"
  [context] (:env context))

(defn symbol-table
  "辅助函数，从上下文获取符号表"
  [context]
  (:symbol-table context))

(defn known-types
  "辅助函数，获取当前所有已知类型"
  [context]
  (:known-types context))

(defn new-env
  "辅助函数，创建新的环境"
  [context env] (assoc context :env env))

(defn add-known-type
  "将 type-sym 添加到上下文的已知类型集合中。"
  [context type-sym]
  (update context :known-types conj type-sym))

(defn success
  "辅助函数，表示推断成功"
  [ty node context] [ty node context])
(defn nothing
  "辅助函数, 表示推断失败"
  [node context] [nil node context])


(defn make-context
  "构建局部推断所需的上下文 map。合并前端内置符号表与编译上下文用户符号表。"
  [compile-ctx frontend backend]
  (let [builtin-table (tp/builtin-symbols frontend)
        user-table    (ip/symbol-table compile-ctx)
        symbols (merge builtin-table user-table)]
    {:frontend frontend
     :backend backend
     :ctx compile-ctx
     :symbol-table (merge builtin-table user-table)
     :known-types (sym/types-symbols symbols)
     :env {}}))


(defn infer
  "局部类型推导Pass, 为后续类型推导创建基础的类型标记. 本Pass 做一下几件事情
  1. 当 :meta 存在类型标记时候(^:float 等, :float 是 IFrontend 协议提供的前端类型列表中的类型).
     将其复制到 :attrs 中 :type 键值，并转化为内部类型表示 TCon.
  2. 在局部上下文进行前向的类型推导. "
  [ir2-roots context]
  (let [[new-roots _] (reduce
                        (fn [[roots ctx] node]
                          (let [[_ new-node new-ctx] (local-infer node ctx)]
                            [(conj roots new-node) new-ctx]))
                        [[] context]
                        ir2-roots)]
    (vec new-roots)))