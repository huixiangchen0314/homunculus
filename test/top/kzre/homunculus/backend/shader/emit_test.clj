(ns top.kzre.homunculus.backend.shader.emit-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [top.kzre.homunculus.backend.shader.emit :as emit]
            [top.kzre.homunculus.backend.shader.methods]
            [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.forms]
            [top.kzre.homunculus.core.types.model :as t])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(def backend (hlsl-backend/->HLSLBackend))

(defn tvar-typed [name ty]
  (m/->VariableNode name {:type ty} nil nil))

(defn lit [val]
  (m/->LiteralNode val nil nil nil))

(defn tvar [name]
  (m/->VariableNode name nil nil nil))

(deftest lambert-simple-test
  (testing "最简单着色器：直接返回红色"
    (let [fn-body (m/->CallNode (tvar "float4")
                                [(lit 1) (lit 0) (lit 0) (lit 1)]
                                nil nil nil)
          body-typed (assoc-in fn-body [:attrs :type] (t/->TCon :float4))
          pos-param    (tvar-typed "pos"    (t/->TCon :float4))
          normal-param (tvar-typed "normal" (t/->TCon :float3))
          lambda (m/->LambdaNode [pos-param normal-param] body-typed [] nil nil nil nil)
          define (m/->DefineNode 'lambert lambda nil nil nil nil)
          roots [define]
          result (emit/generate roots backend [{:stage :fragment :fn-name "lambert"}])]
      (println "Generated HLSL:\n" result)
      (is (s/includes? result "float4 lambert(float4 pos, float3 normal)"))
      (is (s/includes? result "return float4(1, 0, 0, 1);"))
      ;; 现在片段入口是 float4 main() : SV_TARGET { return lambert(); }
      (is (s/includes? result "float4 main() : SV_TARGET { return lambert(); }")))))