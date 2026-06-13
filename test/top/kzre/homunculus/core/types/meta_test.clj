(ns top.kzre.homunculus.core.types.meta-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir1.forms]
            [top.kzre.homunculus.core.ir2.forms]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
            [top.kzre.homunculus.backend.shader.emit :as emit]))

(def backend (hlsl-backend/->HLSLBackend))

;; 测试 1：IR1 符号元数据
(deftest ir1-symbol-meta-test
  (let [sym (with-meta 'x {:foo true})
        node (ir1/->ir1 sym)]
    (is (= :symbol (ir1p/kind node)))
    (is (= {:foo true} (:meta node)))))

;; 测试 2：直接构造 VariableNode，验证 node-meta 可存储和读取
(deftest ir2-variable-meta-manual-test
  (let [var (m/->VariableNode "x" nil {:SV_Position true} nil)]
    (is (= :variable (ir2p/kind var)))
    (is (= {:SV_Position true} (ir2p/node-meta var)))))

;; 测试 3：emit 读取 node-meta 并生成语义字符串
(deftest emit-semantic-test
  (let [param (m/->VariableNode "pos" {:type (t/->TCon :float4)} {:SV_Position true} nil)
        body  (m/->CallNode
                (m/->VariableNode "float4" nil nil nil)
                [(m/->LiteralNode 1.0 nil nil nil)
                 (m/->LiteralNode 0.0 nil nil nil)
                 (m/->LiteralNode 0.0 nil nil nil)
                 (m/->LiteralNode 1.0 nil nil nil)]
                nil nil nil)
        body-typed (assoc body :attrs {:type (t/->TCon :float4)})
        lambda (m/->LambdaNode [param] body-typed [] nil nil nil nil)
        define (m/->DefineNode 'vs-main lambda nil nil nil nil)
        result (emit/emit define backend)]
    (println "Generated:\n" result)
    (is (re-find #"float4 pos\s*:\s*SV_Position" result))))

;; 测试 4：集成测试 - 从 IR1 到 IR2 的元数据传递（需要 lowering 正确注册）
(deftest ir2-variable-meta-integration-test
  (testing "fn* 参数上的元数据应通过 lowering 传递到 IR2 VariableNode"
    ;; 确保 lowering 方法已加载
    (require 'top.kzre.homunculus.core.ir2.forms)
    (let [ir1-root (ir1/->ir1 '(fn* [^:SV_Position x] x))
          ir2-roots (ir2/lower [ir1-root])
          _ (println "Lowered roots count:" (count ir2-roots))
          _ (doseq [r ir2-roots] (println "Root kind:" (ir2p/kind r)))
          lambda (first ir2-roots)]
      (is (= :lambda (ir2p/kind lambda))
          "第一个根节点应为 :lambda（fn* 被正确 lowering）")
      (let [param (first (:params lambda))]
        (is (= :variable (ir2p/kind param)))
        (is (contains? (ir2p/node-meta param) :SV_Position)
            "参数的 node-meta 应包含 :SV_Position")))))

;; 测试 5：defshader 宏展开测试（如果 dsl 命名空间已实现）
#_
        (deftest defshader-macro-test
          (require 'top.kzre.homunculus.backend.shader.dsl)
          (let [form (macroexpand-1 '(top.kzre.homunculus.backend.shader.dsl/defshader
                                       :vertex vs-main
                                       [^:SV_Position ^:float4 pos]
                                       (return pos)))]
            (is (= :vertex (:shader-stage (meta (second form)))))
            (is (= 'vs-main (second form)))))