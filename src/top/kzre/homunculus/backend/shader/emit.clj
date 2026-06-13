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
    (str fn-name "(" (str/join ", " arg-strs) ")")))

;; ── if 语句 ──
(defmethod emit :if [node backend]
  (sp/shader-if backend
                (emit (:test node) backend)
                (emit (:then node) backend)
                (when (:else node)
                  (emit (:else node) backend))))

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
        ;; 为每个绑定生成声明和初始化
        var-decls (map (fn [[var val]]
                         (let [var-name (:name var)
                               var-type (get-in var [:attrs :type])
                               val-code (emit val backend)]
                           (sp/shader-var-decl backend var-name var-type false val-code)))
                       bindings)
        body-code (emit body backend)]
    (str (str/join "\n" var-decls) "\n" body-code)))

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
        ;; 参数声明：类型 + 名字，不带分号
        param-strs (map (fn [p]
                          (let [ty (get-in p [:attrs :type])
                                type-str (if ty (sp/shader-type backend ty) "float")
                                name (sp/shader-var-ref backend (:name p))]
                            (str type-str " " name)))
                        params)
        body-code (emit body backend)
        ;; 若函数体不是 block，自动包裹 return
        return-body (if (and (satisfies? ir2p/INode body)
                             (= (ir2p/kind body) :block))
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
  "对 IR2 根节点列表生成完整的着色器代码。
   backend 为 IShaderBackend 实例，entry-stage 如 :vertex，entry-fn-name 为入口调用的函数名。"
  [ir2-roots backend entry-stage entry-fn-name]
  (let [defines (filter #(= (ir2p/kind %) :define) ir2-roots)
        globals (remove #(= (ir2p/kind %) :define) ir2-roots)
        fn-defs (map #(emit % backend) defines)
        global-code (when (seq globals)
                      (str/join "\n" (map #(emit % backend) globals)))
        body (str/join "\n" fn-defs)
        entry (sp/shader-entry-point backend entry-stage entry-fn-name)]
    (str/join "\n" (filter seq [global-code body entry]))))