(ns top.kzre.homunculus.backend.shader.methods.if
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]
            [top.kzre.homunculus.backend.shader.protocol :as sp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod emit :if [node backend]
  (let [test-code (emit (:test node) backend)      ;; 测试表达式，不含外层括号
        then-code (emit (:then node) backend)
        else-code (when (:else node) (emit (:else node) backend))
        wrap-return (fn [code sub-node]
                      (if (or (not (satisfies? ir2p/INode sub-node))
                              (#{:literal :call :variable :vector :block} (ir2p/kind sub-node)))
                        (sp/shader-return backend code)
                        code))]
    (sp/shader-if backend
                  test-code
                  (wrap-return then-code (:then node))
                  (when else-code
                    (wrap-return else-code (:else node))))))