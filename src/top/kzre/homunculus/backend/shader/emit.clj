(ns top.kzre.homunculus.backend.shader.emit
  "着色器通用代码生成器，基于 IR2 节点种类和 IShaderBackend 协议。"
  (:require [top.kzre.homunculus.backend.shader.protocol :as sp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [clojure.string :as str]
            [top.kzre.homunculus.core.types.model :as t]))

(defmulti emit
          "生成节点对应的着色器代码字符串。"
          (fn [node backend]
            (cond
              (satisfies? ir2p/INode node) (ir2p/kind node)      ;; 正常 IR2 节点
              (map? node)                   (:kind node)          ;; 虚拟节点（如 convert）
              :else                         (throw (ex-info "Unsupported node type" {:node node})))))

(defmethod emit :convert [node backend]
  (sp/shader-cast backend
                  (emit (:expr node) backend)
                  (:src-type (:attrs node))
                  (:dst-type (:attrs node))))
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
    (sp/shader-call backend fn-name arg-strs)))

;; ── if 语句 ──
(defmethod emit :if [node backend]
  (let [test-code (emit (:test node) backend)
        then-code (emit (:then node) backend)
        else-code (when (:else node) (emit (:else node) backend))
        ;; 为简单表达式分支自动加 return
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

;; ── while 循环 ──
(defmethod emit :while [node backend]
  (sp/shader-while backend
                   (emit (:test node) backend)
                   (emit (:body node) backend)))

;; ── block ──
(defmethod emit :block [node backend]
  (let [stmts (:exprs node)
        n (count stmts)
        emitted (map-indexed (fn [i expr]
                               (let [code (emit expr backend)]
                                 (if (and (= i (dec n))
                                          (satisfies? ir2p/INode expr)
                                          (#{:literal :call :variable :vector} (ir2p/kind expr)))
                                   (sp/shader-return backend code)
                                   code)))
                             stmts)]
    (str/join "\n" emitted)))

;; ── let 绑定 ──
(defmethod emit :let [node backend]
  (let [bindings (:bindings node)
        body     (:body node)
        var-decls (map (fn [[var val]]
                         (let [var-name (:name var)
                               var-type (get-in var [:attrs :type])
                               mutable? (get-in var [:attrs :mutable])  ;; 读取可变性
                               val-code (emit val backend)]
                           (sp/shader-var-decl backend var-name var-type (boolean mutable?) val-code)))
                       bindings)
        body-code (emit body backend)
        body-final (if (or (not (satisfies? ir2p/INode body))
                           (#{:literal :call :variable :vector} (ir2p/kind body)))
                     (sp/shader-return backend body-code)
                     body-code)]
    (str (str/join "\n" var-decls) "\n" body-final)))

;; ── 赋值 ──
(defmethod emit :assign [node backend]
  (sp/shader-assign backend
                    (emit (:var node) backend)
                    (emit (:val node) backend)))

;; ── 函数定义 ──
(defmethod emit :define [node backend]
  (let [val (:val node)
        params (:params val)
        body   (:body val)
        param-strs (map (fn [p]
                          (let [ty (get-in p [:attrs :type])
                                type-str (if ty (sp/shader-type backend ty) "float")
                                param-name (sp/shader-var-ref backend (:name p))  ;; 局部改名
                                metadata (ir2p/node-meta p)
                                semantic (when (map? metadata)
                                           (some (fn [k]
                                                   (when (and (keyword? k)
                                                              (not (namespace k))
                                                              ( Character/isUpperCase  ^char (first (name k))))
                                                     k))
                                                 (keys metadata)))]
                            (str type-str " " param-name
                                 (if semantic (str " : " (name semantic)) ""))))
                        params)
        body-code (emit body backend)
        return-body (if (or (= (ir2p/kind body) :block)
                            (#{:if :while :let :loop :assign :throw} (ir2p/kind body)))
                      body-code
                      (sp/shader-return backend body-code))
        return-type (if-let [rt (get-in body [:attrs :type])]
                      (sp/shader-type backend rt)
                      "void")]
    (sp/shader-function-decl backend (:name node) param-strs return-type return-body)))

;; ── lambda 节点（不应独立出现，但防御性处理）──
(defmethod emit :lambda [node backend]
  (throw (ex-info "Lambda node should not appear in final IR2; elaborate pass may have missed it." {:node node})))

;; ── loop/recur 已被消除，若出现则报错 ──
(defmethod emit :loop [node backend]
  (throw (ex-info ":loop node should have been eliminated by recur-elim pass." {:node node})))

(defmethod emit :recur [node backend]
  (throw (ex-info ":recur node should have been eliminated by recur-elim pass." {:node node})))

;; ── try/catch/throw（着色器不支持异常）──
(defmethod emit :try [node backend]
  (throw (ex-info "Try/catch is not supported in shader languages." {:node node})))

(defmethod emit :catch [node backend]
  (throw (ex-info "Catch is not supported in shader languages." {:node node})))

(defmethod emit :throw [node backend]
  ;; 可选择生成 discard 语句，但此处直接报错
  (throw (ex-info "Throw is not supported in shader languages." {:node node})))

;; ── 向量字面量（转换为构造函数调用）──
(defmethod emit :vector [node backend]
  (let [items (:items node)
        elem-type (or (some-> (first items) (get-in [:attrs :type]))
                      (t/->TCon :float))
        elem-name (name (:name elem-type))          ;; 如 "float"
        type-str (str elem-name (count items))      ;; "float4"
        args (map #(emit % backend) items)]
    (str type-str "(" (str/join ", " args) ")")))

;; ── 映射字面量（着色器不支持）──
(defmethod emit :map [node backend]
  (throw (ex-info "Map literals are not supported in shader languages." {:node node})))

;; ── 类型转换节点 ──
(defmethod emit :convert [node backend]
  (sp/shader-cast backend
                  (emit (:expr node) backend)
                  (:src-type (:attrs node))
                  (:dst-type (:attrs node))))

;; ── 默认方法：捕获未处理的节点类型 ──
(defmethod emit :default [node backend]
  (throw (ex-info (str "Unhandled node: " node) {:node node})))

;; ══════════════════════════════════════════
;; 顶层生成入口
;; ══════════════════════════════════════════
;; backend/shader/emit.clj 中增加辅助函数和修改 generate

(defn- param-semantic [p]
  (let [meta (ir2p/node-meta p)]
    (some (fn [k] (when (and (keyword? k) ( Character/isUpperCase ^char (first (name k)))) k))
          (keys meta))))

(defn- generate-vertex-entry [fn-name params backend]
  (let [call-args (map (fn [p] (str "input." (sp/shader-var-ref backend (:name p)))) params)]
    (str "VSOutput main(VSInput input) {\n"
         "    VSOutput output;\n"
         "    output.pos = " fn-name "(" (clojure.string/join ", " call-args) ");\n"
         "    return output;\n"
         "}")))

(defn- generate-fragment-entry [fn-name params backend]
  (let [output-param (first (filter #(= :SV_Target (param-semantic %)) params))
        output-type (if output-param
                      (let [ty (get-in output-param [:attrs :type])]
                        (if ty (sp/shader-type backend ty) "float4"))
                      "float4")]
    (str output-type " main() : SV_TARGET {\n"
         "    return " fn-name "();\n"
         "}")))

(defn generate
  [ir2-roots backend entry-stage entry-fn-name]
  (let [defines (filter #(= (ir2p/kind %) :define) ir2-roots)
        fn-defs (map #(emit % backend) defines)
        globals []
        struct-defs (if (seq defines)
                      (let [main-define (first defines)
                            lambda (:val main-define)
                            params (:params lambda)]
                        (case entry-stage
                          :vertex (let [inputs (remove #(= :SV_Position (param-semantic %)) params)
                                        output-param (first (filter #(= :SV_Position (param-semantic %)) params))
                                        output-type (if output-param
                                                      (sp/shader-type backend (get-in output-param [:attrs :type]))
                                                      "float4")]
                                    (remove nil?
                                            [(when (seq inputs)
                                               (sp/shader-struct-decl backend "VSInput"
                                                                      (mapv (fn [p] {:name (sp/shader-var-ref backend (:name p))
                                                                                     :type (sp/shader-type backend (get-in p [:attrs :type]))
                                                                                     :semantic (some-> (param-semantic p) name)})
                                                                            inputs)))
                                             (sp/shader-struct-decl backend "VSOutput"
                                                                    [{:name "pos"
                                                                      :type output-type
                                                                      :semantic "SV_POSITION"}])]))
                          :fragment [] ;; 片元后续补充
                          []))
                      [])
        entry (if (seq defines)
                (let [main-define (first defines)
                      fn-name (sp/shader-var-ref backend (:name main-define))
                      lambda (:val main-define)
                      params (:params lambda)]
                  (case entry-stage
                    :vertex   (generate-vertex-entry fn-name params backend)
                    :fragment (generate-fragment-entry fn-name params backend)
                    (str "void main() { " fn-name "(); }")))
                ;; 理论上不会到这里，因为 compile-and-emit 保证有 define
                "")]
    (sp/shader-program backend fn-defs struct-defs globals entry)))