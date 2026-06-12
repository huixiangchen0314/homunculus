(ns top.kzre.homunculus.backend.shader.emit
  "着色器通用代码生成器，基于 IR2 节点种类和 IShaderBackend 协议。"
  (:require
   [clojure.string :as s]
   [top.kzre.homunculus.backend.shader.protocol :as sp]
   [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmulti emit
          "生成节点对应的着色器代码字符串。"
          (fn [node backend] (ir2p/kind node)))

;; ── 字面量 ──
(defmethod emit :literal [node backend]
  (sp/shader-literal backend (:val node)))

;; ── 变量引用 ──
(defmethod emit :variable [node backend]
  (sp/shader-var-ref backend (:name node)))

;; ── 函数调用 ──
(defmethod emit :call [node backend]
  (let [fn-node (:fn node)
        args    (:args node)
        fn-name (emit fn-node backend)
        arg-strs (map #(emit % backend) args)]
    (str fn-name "(" (s/join ", " arg-strs) ")")))

;; ── let 绑定 ──
(defmethod emit :let [node backend]
  (let [bindings (:bindings node)
        body     (:body node)
        ;; 为每个绑定生成声明和初始化
        var-decls (map (fn [[var val]]
                         (let [var-name (:name var)
                               var-type (get-in var [:attrs :type]) ;; 可能需要默认
                               val-code (emit val backend)]
                           (sp/shader-var-decl backend var-name var-type false val-code)))
                       bindings)
        body-code (emit body backend)]
    (str (s/join "\n" var-decls) "\n" body-code)))

;; ── block ──
(defmethod emit :block [node backend]
  (let [stmts (map #(emit % backend) (:exprs node))]
    (s/join ";\n" stmts)))

;; ── 函数定义 ──
(defmethod emit :define [node backend]
  (let [val (:val node)
        params (:params val)
        body   (:body val)
        ;; 参数声明：类型 + 名字，不带 const（HLSL 参数不可变）
        param-strs (map (fn [p]
                          (let [ty (get-in p [:attrs :type])
                                type-str (if ty (sp/shader-type backend ty) "float")
                                name (sp/shader-var-ref backend (:name p))]
                            (str type-str " " name)))
                        params)
        body-code (emit body backend)
        ;; 如果函数体不是 block，则自动包裹 return
        return-body (if (and (satisfies? ir2p/INode body)
                             (= (ir2p/kind body) :block))
                      body-code
                      (sp/shader-return backend body-code))
        return-type (if-let [rt (get-in body [:attrs :type])]
                      (sp/shader-type backend rt)
                      "void")]
    (sp/shader-function-decl backend (:name node) param-strs return-type return-body)))

;; ── 入口函数包装 ──
(defn generate
  [ir2-roots backend entry-stage entry-fn-name]
  (let [defines (filter #(= (ir2p/kind %) :define) ir2-roots)
        bodies  (map #(emit % backend) defines)
        entry   (sp/shader-entry-point backend entry-stage entry-fn-name)]
    (str (s/join "\n" bodies) "\n" entry)))