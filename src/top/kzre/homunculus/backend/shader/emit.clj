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
  (let [stmts (map #(emit % backend) (:exprs node))]
    (str/join ";\n" stmts)))

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
                                name (sp/shader-var-ref backend (:name p))]
                            (str type-str " " name)))
                        params)
        body-code (emit body backend)
        ;; 若体是 block 或控制流，不加 return；否则自动包裹 return
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
(defn generate
  [ir2-roots backend entry-stage entry-fn-name]
  (let [defines (filter #(= (ir2p/kind %) :define) ir2-roots)
        others  (remove #(= (ir2p/kind %) :define) ir2-roots)
        fn-defs (map #(emit % backend) defines)
        other-code (when (seq others)
                     (let [exprs (map #(emit % backend) others)]
                       (if (= 1 (count exprs))
                         (sp/shader-return backend (first exprs))
                         (sp/shader-block backend exprs))))
        body (str/join "\n" (remove nil? [other-code (str/join "\n" fn-defs)]))
        entry (sp/shader-entry-point backend entry-stage entry-fn-name)]
    (str/join "\n" (filter seq [body entry]))))