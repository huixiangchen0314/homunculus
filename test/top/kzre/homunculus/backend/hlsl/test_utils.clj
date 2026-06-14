;; test/top/kzre/homunculus/backend/hlsl/test_utils.clj
(ns top.kzre.homunculus.backend.hlsl.test-utils
  "HLSL 集成测试公共工具，提供统一的编译和发射函数。"
  (:require [clojure.walk :as walk]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.types.recur-elim.core :as recur-elim]
            [top.kzre.homunculus.core.types.elaborate.core :as elaborate]
            [top.kzre.homunculus.core.types.mutability.core :as mut]
            [top.kzre.homunculus.core.types.builtin-check.core :as builtin]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
            [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
            [top.kzre.homunculus.backend.shader.emit :as emit]
            [top.kzre.homunculus.core.types.elaborate.protocol :as elab-cfg]
            [top.kzre.homunculus.backend.hlsl.test-utils :refer [elab-config]]))

(def elab-config
  "共享的 elaborate 配置，与原始 integration_test 保持一致。"
  (reify elab-cfg/IElaborateConfig
    (max-iterations [_] 5)
    (strict-mode? [_] true)
    (allow-return-closure? [_] false)
    (on-unresolved [_ lambda] (throw (ex-info "Unresolved closure" {:lambda lambda})))
    (should-inline? [_ _ _] true)))

(def hlsl-frontend (hlsl-front/->HLSLFrontend))
(def hlsl-backend-real (hlsl-backend/->HLSLBackend))
(def full-builtins (merge {} hlsl-front/builtins))

(defn compile-and-emit
  "编译 Clojure 形式并生成 HLSL 代码。
   entries 为入口描述列表，每个元素为 {:stage :vertex/:fragment, :fn-name \"...\"}。
   默认使用真实 HLSL 后端。可以通过 :backend 参数覆盖。"
  ([form entries]
   (compile-and-emit form entries {}))
  ([form entries {:keys [backend]}]
   (let [backend (or backend hlsl-backend-real)
         expanded (walk/macroexpand-all form)
         ir1-root (ir1/->ir1 expanded)
         ir2-roots (ir2/lower [ir1-root])
         no-recur (mapv recur-elim/eliminate ir2-roots)
         elaborated (elaborate/elaborate no-recur elab-config)
         mutable (mut/analyze elaborated)
         checked-fn (builtin/check mutable full-builtins)
         inferred (infer/run checked-fn :frontend hlsl-frontend)
         typed (typed/type-check inferred :frontend hlsl-frontend :builtins full-builtins)
         checked (check/check-program typed {:backend backend})]
     (emit/generate checked backend entries))))