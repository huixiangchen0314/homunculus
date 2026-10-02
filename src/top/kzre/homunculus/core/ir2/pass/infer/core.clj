(ns top.kzre.homunculus.core.ir2.pass.infer.core
  "轻量级局部类型推导 pass（前向传播）。

   使用与约束求解相同的类型环境（gen.env/IEnv）——
   三个类型推导 pass 共享同一套对外部世界的访问方式。"
  (:require
    [top.kzre.homunculus.core.ir2.ast :as ir2p]))

(defmulti infer-node*
          "对节点树递归进行局部类型推导。
           返回三元组 [type new-node env]：
           - type      当前节点推断类型，推断失败返回 nil
           - new-node  重建后的当前节点（含子节点）
           - env       更新后的类型环境"
          (fn [node _env] (ir2p/kind node)))

;; ── 返回值构造 ──────────────────────────

(defn success
  "推断成功。"
  [type node env]
  [type node env])

(defn nothing
  "推断失败（类型为 nil），但保留节点和环境。"
  [node env]
  [nil node env])

;; ── 默认遍历 ────────────────────────────

(defn- infer-fn
  [node env]
  (let [[_ new-node new-env] (infer-node* node env)]
    [new-node new-env]))

(defmethod infer-node* :default [node env]
  (let [[new-node new-env] (ir2p/reduce-children node infer-fn env)]
    [nil new-node new-env]))

;; ── 入口 ────────────────────────────────

(defn infer
  "局部类型推导 Pass，为后续类型推导创建基础的类型标记。

   本 Pass 做以下事情：
   1. 当 :meta 存在类型标记（^:float 等）时，转化为 TCon 写入 :attrs :type。
   2. 在局部上下文进行前向类型推导。

   调用方负责用 gen.env/make-env 构造初始 env。"
  [ir2-roots init-env]
  (let [[new-roots _]
        (reduce
          (fn [[roots env] node]
            (let [[_ new-node new-env] (infer-node* node env)]
              [(conj roots new-node) new-env]))
          [[] init-env]
          ir2-roots)]
    (vec new-roots)))