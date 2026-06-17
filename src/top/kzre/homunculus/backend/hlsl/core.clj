(ns top.kzre.homunculus.backend.hlsl.core
  "HLSL 代码发射核心：多方法分派与公共辅助。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
    [top.kzre.homunculus.backend.shader.core :as sc]
    [top.kzre.homunculus.backend.shader.types :as st]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.core.types.metadata :as md]))

;; ── 类型转换适配 ──
(defn hlsl-type-str [ir-type]
  (st/shader-type-str (ty/type-sym ir-type)))

;; ── 多方法分发 ──
(defmulti emit-node (fn [node] (n/kind node)))

(defmethod emit-node :default [node]
  (throw (ex-info (str "HLSL emit not implemented for " (n/kind node)) {:node node})))

;; ── 入口包装（使用 shader.core）──
(defn- emit-entry-wrapper [stage define-node]
  (let [{:keys [input-params output-params]} (sc/entry-spec stage define-node hlsl-type-str)
        func-name (name (n/define-name define-node))
        input-struct-name (str func-name "_Input")
        output-struct-name (str func-name "_Output")
        input-members  (str/join "\n" (mapv (fn [p] (tmpl/struct-member (:type p) (:name p) (:semantic p))) input-params))
        output-members (str/join "\n" (mapv (fn [p] (tmpl/struct-member (:type p) (:name p) (:semantic p))) output-params))
        input-struct  (tmpl/struct-decl input-struct-name input-members)
        output-struct (tmpl/struct-decl output-struct-name output-members)
        call-args    (str/join ", " (mapv (fn [p] (str "in." (:name p))) input-params))
        core-call    (str func-name "(" call-args ")")
        wrapper-body (if (= stage :vertex)
                       (str "VSOutput out;\n"
                            "out.position = " core-call ";\n"
                            "return out;")
                       (str "return " core-call ";"))]
    (str input-struct "\n"
         output-struct "\n"
         (tmpl/entry-wrapper stage func-name input-struct-name output-struct-name wrapper-body))))

;; ── 公共入口 ──
(defn emit
  "对 IR2 根节点列表发射 HLSL 代码。"
  [ir2-roots]
  (let [flat      (mapcat n/unwrap-body ir2-roots)
        defines   (filter n/define-node? flat)
        {:keys [resources globals functions]} (sc/classify-defines defines)
        resource-strs  (mapv (fn [d] (emit-node (assoc d :kind :define-resource))) resources)
        global-strs    (mapv emit-node globals)
        fn-strs        (mapv emit-node functions)
        entry-fns      (filter #(md/fn-shader-stage %) functions)
        entry-wrappers (mapv (fn [d] (emit-entry-wrapper (md/fn-shader-stage d) d)) entry-fns)]
    (str/join "\n\n" (concat resource-strs global-strs fn-strs entry-wrappers))))