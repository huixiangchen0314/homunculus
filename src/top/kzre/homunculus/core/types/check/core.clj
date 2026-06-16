(ns top.kzre.homunculus.core.types.check.core
  "类型检查 pass：利用 typed-pass 的结果和后端信息进行双向检查，插入类型转换。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmulti check-node
          "对节点进行双向检查。expected 为期望类型（可为 nil）。
           返回更新后的节点（可能被 :convert 包裹）。"
          (fn [node expected context] (ir2p/kind node)))

;; ── 辅助：创建转换节点 ──
(defn- make-convert [node src dst cost]
  (n/make-convert node src dst cost
                  {:type dst :src-type src :cost cost}
                  (n/node-meta node)
                  (n/parent node)))

;; ── 辅助：尝试转换 ──
(defn- try-convert [node actual expected context]
  (let [backend (:backend context)]
    (if-let [cost (and backend (tp/type-conversion backend actual expected))]
      (make-convert node actual expected cost)
      (throw (ex-info (str "Type mismatch: expected " expected ", got " actual)
                      {:node node :expected expected :actual actual})))))

;; ── 通用类型检查 ──
(defn check-type
  "若 expected 非 nil 且实际类型不兼容，则尝试转换或报错。
   如果实际类型是 TScheme，则先实例化再比较（不改变节点本身类型）。"
  [node expected context]
  (let [actual (ty/get-type node)
        actual* (if (ty/scheme-type? actual)
                  (scheme/instantiate actual)
                  actual)]
    (if (or (nil? expected) (= actual* expected) (ty/var-type? actual*))
      node
      (try-convert node actual* expected context))))

;; ── 入口 ──
(defn check-program
  "检查 IR2 根节点序列（顶层无期望类型）。"
  [ir2-roots context]
  (mapv #(check-node % nil context) ir2-roots))