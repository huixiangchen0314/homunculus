(ns top.kzre.homunculus.core.types.check.core
  "类型检查 pass：利用 typed‑pass 的结果和后端信息进行双向检查，插入类型转换。"
  (:require [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(defmulti check
          "对节点进行双向检查。expected 为期望类型（可为 nil）。
           返回更新后的节点（可能被 :convert 包裹）。"
          (fn [node expected context] (ir2p/kind node)))

;; 辅助：创建转换节点
(defn- make-convert [node src dst cost]
  {:kind :convert
   :expr node
   :attrs {:type dst :src-type src :cost cost}})

;; 辅助：尝试转换
(defn- try-convert [node actual expected context]
  (let [backend (:backend context)]
    (if-let [cost (and backend (tp/type-conversion backend actual expected))]
      ;; 允许转换
      (make-convert node actual expected cost)
      ;; 不允许转换 -> 错误
      (throw (ex-info (str "Type mismatch: expected " expected ", got " actual)
                      {:node node :expected expected :actual actual})))))

;; 通用检查：若 expected 非 nil 且实际类型与 expected 不同，尝试转换或报错

(defn check-type [node expected context]
  (let [actual (get-in node [:attrs :type])]
    (if (or (nil? expected) (= actual expected) (instance? TVar actual))
      node   ;; 允许未确定的类型变量通过
      (try-convert node actual expected context))))

(defn check-program
  "入口：检查 IR2 根节点序列（顶层无期望类型）。"
  [ir2-roots context]
  (mapv #(check % nil context) ir2-roots))