;; top.kzre.homunculus.core.types.infer.core.clj
(ns top.kzre.homunculus.core.types.infer.core
  "轻量级局部类型推导 pass（前向传播）。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.env :as e]))

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

(defn new-env
  "辅助函数，从上下文获取环境map"
  [context env] (assoc context :env env))

(defn success
  "辅助函数，表示推断成功"
  [ty node] [ty node])
(defn nothing
  "辅助函数, 表示推断失败"
  [node] [nil node])

(defn infer
  "局部类型推导Pass, 为后续类型推导创建基础的类型标记. 本Pass 做一下几件事情
  1. 当 :meta 存在类型标记时候(^:float 等, :float 是 IFrontend 协议提供的前端类型列表中的类型).
     将其复制到 :attrs 中 :type 键值，并转化为内部类型表示 TCon.
  2. 在局部上下文进行前向的类型推导. "
  [ir2-roots & {:keys [frontend]}]
  (let [context {:frontend frontend
                 :env {}                                    ;; 局部推断不受上下文影响
                 }
        infer-seq (fn infer-seq [env nodes]
                    (when-let [root (first nodes)]
                      (let [[type new-root] (local-infer root
                                                       (assoc context :env env)) ;;更新上下文并执行局部推导
                            new-env (if (and type
                                             (n/define-node? new-root))
                                      (e/extend-env env (:name new-root) type)
                                      env)]
                        (cons new-root (lazy-seq            ;; 惰性推断剩余节点
                                         (infer-seq new-env (rest nodes)))))))]
    (doall (infer-seq                                       ;; 顺序执行，不支持全局符号查找
             {} ir2-roots))))