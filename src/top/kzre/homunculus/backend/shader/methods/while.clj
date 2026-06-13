(ns top.kzre.homunculus.backend.shader.methods.while
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]
            [top.kzre.homunculus.backend.shader.protocol :as sp]))

(defmethod emit :while [node backend]
  (let [test-code (emit (:test node) backend)      ;; 测试表达式，不含外层括号
        body-code (emit (:body node) backend)]
    (sp/shader-while backend test-code body-code)))