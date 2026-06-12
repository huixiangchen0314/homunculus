(ns top.kzre.homunculus.backend.util.lint
  "静态检查工具（lint）。现阶段为占位，后续实现作用域检查等。")

(defn warn-undefined
  "示例：检查已使用但未声明的变量。实际实现需配合环境上下文。"
  [used-vars declared-vars]
  (let [undefined (remove declared-vars used-vars)]
    (when (seq undefined)
      (println "Warning: undefined variables:" (clojure.string/join ", " undefined)))))