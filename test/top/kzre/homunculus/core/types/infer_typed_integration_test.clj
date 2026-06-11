(ns top.kzre.homunculus.core.types.infer-typed-integration-test
  "测试 infer‑pass → typed‑pass 的串联效果。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods]   ;; 注册 infer 多方法
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]   ;; 注册 typed 多方法
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

;; ── 模拟前端 ──────────────────────────────
(defrecord MockFrontend []
  tp/IFrontendInfo
  (frontend-types [_] [:int64 :float64 :bool :string :keyword :nil])
  (literal->type [_ val]
    (cond
      (instance? java.lang.Long val)    (t/->TCon :int64)
      (instance? java.lang.Double val)  (t/->TCon :float64)
      (instance? java.lang.Boolean val) (t/->TCon :bool)
      (instance? java.lang.String val)  (t/->TCon :string)
      (keyword? val)                    (t/->TCon :keyword)
      (nil? val)                        (t/->TCon :nil)
      :else (throw (ex-info "Unknown literal" {:val val}))))
  (meta->type [_ node]
    (when-let [tag (or (get-in node [:meta :tag])
                       (get-in node [:attrs :tag]))]
      (if (keyword? tag)
        (t/->TCon tag)
        (t/->TCon (keyword (name tag))))))
  (infer-collection-type [_ form] nil)
  (collection-type-ctor [_ kind element-type shape] nil))

(defn- tcon? [ty name] (and (instance? TCon ty) (= name (:name ty))))

(deftest let-binding-infer-typed-chain
  (let [frontend (->MockFrontend)
        ;; 构造 IR2 节点：(let [x 42] x)
        val-node (m/->LiteralNode 42 nil nil [] nil)
        var-node (m/->VariableNode "x" nil nil [] nil)
        body-node (m/->VariableNode "x" nil nil [] nil)
        let-node (m/->LetNode [[var-node val-node]] body-node nil nil [] nil)

        ;; 第一步：infer‑pass 推导
        infer-result (first (infer/run [let-node] :frontend frontend))

        ;; 第二步：typed‑pass 推导（复用 infer 结果）
        typed-results (typed/type-check [infer-result] :frontend frontend)
        final-node (first typed-results)]
    (testing "最终类型应为 int64"
      (is (tcon? (-> final-node ir2p/attrs :type) :int64))
      ;; body 变量 x 的类型也应是 int64，并且来自 infer（不需重新推导）
      (let [body-x (-> final-node :body)]
        (is (tcon? (-> body-x ir2p/attrs :type) :int64))))))