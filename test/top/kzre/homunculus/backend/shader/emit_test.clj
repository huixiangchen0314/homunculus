(ns top.kzre.homunculus.backend.shader.emit-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [top.kzre.homunculus.backend.shader.emit :as emit]
            [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.model :as t])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(def backend (hlsl-backend/->HLSLBackend))

;; 辅助函数：创建带类型标注的变量
(defn tvar-typed [name ty]
  (m/->VariableNode name {:type ty} nil nil))

;; 辅助函数：创建字面量
(defn lit [val]
  (m/->LiteralNode val nil nil nil))

;; 辅助函数：创建普通变量引用（无类型）
(defn tvar [name]
  (m/->VariableNode name nil nil nil))

(deftest lambert-simple-test
  (testing "最简单着色器：直接返回红色"
    (let [;; 函数体：float4(1,0,0,1)
          fn-body (m/->CallNode (tvar "float4")
                                [(lit 1) (lit 0) (lit 0) (lit 1)]
                                nil nil nil)
          ;; 标记返回类型
          body-typed (assoc-in fn-body [:attrs :type] (t/->TCon :float4))
          ;; 参数列表：pos(float4), normal(float3)
          pos-param    (tvar-typed "pos"    (t/->TCon :float4))
          normal-param (tvar-typed "normal" (t/->TCon :float3))
          ;; 构建 lambda 和 define
          lambda (m/->LambdaNode [pos-param normal-param] body-typed [] nil nil nil nil)
          define (m/->DefineNode 'lambert lambda nil nil nil nil)
          roots [define]
          ;; 生成 HLSL
          result (emit/generate roots backend [{:stage :fragment :fn-name "lambert"}])]
      (println "Generated HLSL:\n" result)
      ;; 验证关键部分
      (is (s/includes? result "float4 lambert(float4 pos, float3 normal)"))
      (is (s/includes? result "return float4(1, 0, 0, 1);"))
      (is (s/includes? result "void main()")))))