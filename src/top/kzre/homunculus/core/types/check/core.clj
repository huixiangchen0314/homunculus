(ns top.kzre.homunculus.core.types.check.core
  "类型检查 pass：利用 typed-pass 的结果和后端信息进行双向检查，插入类型转换。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.type :as ty]
            [top.kzre.homunculus.internal.protocol :as ip]
            [top.kzre.homunculus.internal.symbol :as sym]))

(defmulti check-node
          "对节点进行双向检查。expected 为期望类型（可为 nil）。
           返回更新后的节点（可能被 :convert 包裹）。"
          (fn [node _expected _context] (ir2p/kind node)))

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

(defn make-context
  "构建类型检查上下文，合并前端与用户符号表，并提取已知类型集合。"
  [compile-ctx frontend backend]
  (let [builtin-table (tp/builtin-symbols frontend)
        user-table    (ip/symbol-table compile-ctx)
        symbols       (merge builtin-table user-table)]
    {:ctx compile-ctx
     :frontend frontend
     :backend backend
     :symbol-table symbols
     :known-types (sym/types-symbols symbols)}))

;; ── 上下文访问工具 ──
(defn frontend [ctx] (:frontend ctx))
(defn integer-type [ctx]
  (tp/integer-type (frontend ctx)))
(defn backend [ctx] (:backend ctx))
(defn compile-ctx [ctx] (:ctx ctx))
(defn symbol-table [ctx] (:symbol-table ctx))
(defn known-types [ctx] (:known-types ctx))


;; ── 通用类型检查（修正版）──
(defn check-type
  "检查节点实际类型是否与期望类型兼容。
   - 若 expected 为 nil，直接放行。
   - 若节点无确定类型（nil 或类型变量），报错。
   - 否则进行类型比较，不兼容时尝试隐式转换或报错。"
  [node expected context]
  (if (nil? expected)
    node
    (let [actual (ty/get-type node (:known-types context))
          actual* (if (ty/scheme-type? actual)
                    (scheme/instantiate actual)
                    actual)]
      (when (or (nil? actual*)
                (ty/var-type? actual*))
        (throw (ex-info (str "Node type is not determined")
                        {:node node :actual actual})))
      (if (= actual* expected)
        node
        (try-convert node actual* expected context)))))

;; ── 入口 ──
(defn check-program
  "检查 IR2 根节点序列（顶层无期望类型）。"
  [ir2-roots context]
  (mapv #(check-node % nil context) ir2-roots))