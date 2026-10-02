(ns top.kzre.homunculus.core.ir2.pass.check.core
  "类型检查 pass：利用 typed-pass 的结果和后端信息进行双向检查，插入类型转换。"
  (:require
    [top.kzre.homunculus.core.error :as err]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.ast :as ir2p]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmulti check-node*
          "对节点进行双向检查。expected 为期望类型（可为 nil）。
           返回更新后的节点（可能被 :convert 包裹）。"
          (fn [node _expected _env] (ir2p/kind node)))

;; ── 辅助：创建转换节点 ──
(defn- make-convert [node src dst cost]
  (n/make-convert node src dst
                  {:type dst :src-type src :cost cost}
                  (n/node-meta node)))

;; ── 辅助：尝试转换 ──
(defn- try-convert [node actual expected env]
  (if-let [cost (p/conversion-cost env actual expected)]
    (make-convert node actual expected cost)
    (throw (err/compile-error
             :type-mismatch
             (str "Type mismatch: expected " expected ", got " actual)
             {:node node :expected expected :actual actual}))))

;; ── 通用类型检查 ──
(defn check-type
  "检查节点实际类型是否与期望类型兼容。
   - 节点 actual 必须已确定（nil / var 都是错误）
   - expected 为 nil 时，只验证 actual 已确定
   - expected 非 nil 时，不匹配则尝试转换或报错"
  [node expected env]
  (let [actual  (ty/get-type node)
        actual* (if (ty/scheme-type? actual)
                  (scheme/instantiate actual)
                  actual)]
    (when (or (nil? actual*)
              (not (ty/concrete? actual*)))
      (throw (err/compile-error
               :type-not-determined
               "Node type is not determined"
               {:node node :actual actual})))
    (if (nil? expected)
      node
      (if (= actual* expected)
        node
        (try-convert node actual* expected env)))))

;; ── 入口 ──
(defn check
  "检查 IR2 根节点序列（顶层无期望类型）。"
  [ir2-roots env]
  (mapv #(check-node* % nil env) ir2-roots))