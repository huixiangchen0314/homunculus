(ns top.kzre.homunculus.backend.shader.emit
  "着色器通用代码生成器，基于 IR2 节点种类和 IShaderBackend 协议。"
  (:require [top.kzre.homunculus.backend.shader.protocol :as sp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [clojure.string :as str]
            [top.kzre.homunculus.core.types.model :as t]))

(defmulti emit
          (fn [node backend]
            (cond
              (satisfies? ir2p/INode node) (ir2p/kind node)
              (map? node)                   (:kind node)
              :else                         (throw (ex-info "Unsupported node type" {:node node})))))

(defmethod emit :convert [node backend]
  (sp/shader-cast backend
                  (emit (:expr node) backend)
                  (:src-type (:attrs node))
                  (:dst-type (:attrs node))))

(defmethod emit :literal [node backend]
  (sp/shader-literal backend (:val node)))

(defmethod emit :variable [node backend]
  (sp/shader-var-ref backend (:name node)))

(defmethod emit :call [node backend]
  (let [fn-node (:fn node)
        args    (:args node)
        fn-name (emit fn-node backend)
        arg-strs (map #(emit % backend) args)]
    (sp/shader-call backend fn-name arg-strs)))

(defmethod emit :if [node backend]
  (let [test-code (emit (:test node) backend)
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

(defmethod emit :while [node backend]
  (let [test-code (emit (:test node) backend)
        body-code (emit (:body node) backend)]
    (sp/shader-while backend test-code body-code)))

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
    (str/join ";\n" emitted)))          ;; 用分号连接，每个语句独立

(defmethod emit :let [node backend]
  (let [bindings (:bindings node)
        body     (:body node)
        var-decls (map (fn [[var val]]
                         (let [var-name (:name var)
                               var-type (get-in var [:attrs :type])
                               mutable? (get-in var [:attrs :mutable])
                               val-code (emit val backend)]
                           (sp/shader-var-decl backend var-name var-type (boolean mutable?) val-code)))
                       bindings)
        body-code (emit body backend)
        body-final (if (or (not (satisfies? ir2p/INode body))
                           (#{:literal :call :variable :vector} (ir2p/kind body)))
                     (sp/shader-return backend body-code)
                     body-code)]
    (str (str/join "\n" var-decls) "\n" body-final)))

(defmethod emit :assign [node backend]
  (sp/shader-assign backend
                    (emit (:var node) backend)
                    (emit (:val node) backend)))

(defmethod emit :define [node backend]
  (let [meta (ir2p/node-meta node)]
    (if (:resource-type meta)
      (let [res-type (:resource-type meta)
            name (sp/shader-var-ref backend (:name node))
            reg (:resource-register meta)]
        (case res-type
          :texture2D (str "Texture2D<float4> " name (when reg (str " : register(" reg ")")) ";")
          :sampler   (str "SamplerState " name (when reg (str " : register(" reg ")")) ";")
          :cbuffer   (let [members (:resource-members meta)
                           member-str (clojure.string/join "\n    "
                                                           (map (fn [m] (str (sp/shader-type backend (t/->TCon (:type m))) " " (:name m) ";")) members))]
                       (str "cbuffer " name (when reg (str " : register(" reg ")")) " {\n    " member-str "\n};"))
          (throw (ex-info "Unknown resource type" {:type res-type}))))
      (let [val (:val node)
            params (:params val)
            body   (:body val)
            param-strs (map (fn [p]
                              (let [ty (get-in p [:attrs :type])
                                    type-str (if ty (sp/shader-type backend ty) "float")
                                    param-name (sp/shader-var-ref backend (:name p))
                                    metadata (ir2p/node-meta p)
                                    semantic (when (map? metadata)
                                               (some (fn [k]
                                                       (when (and (keyword? k)
                                                                  (not (namespace k))
                                                                  (re-find #"^[A-Z]" (name k)))
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
        (sp/shader-function-decl backend (:name node) param-strs return-type return-body)))))

(defmethod emit :lambda [node backend]
  (throw (ex-info "Lambda node should not appear in final IR2" {:node node})))

(defmethod emit :loop [node backend]
  (throw (ex-info ":loop should have been eliminated" {:node node})))

(defmethod emit :recur [node backend]
  (throw (ex-info ":recur should have been eliminated" {:node node})))

(defmethod emit :try [node backend]
  (throw (ex-info "Try/catch unsupported in shader" {:node node})))

(defmethod emit :catch [node backend]
  (throw (ex-info "Catch unsupported in shader" {:node node})))

(defmethod emit :throw [node backend]
  (throw (ex-info "Throw unsupported in shader" {:node node})))

(defmethod emit :vector [node backend]
  (let [items (:items node)
        elem-type (or (some-> (first items) (get-in [:attrs :type]))
                      (t/->TCon :float))
        elem-name (name (:name elem-type))
        type-str (str elem-name (count items))
        args (map #(emit % backend) items)]
    (str type-str "(" (str/join ", " args) ")")))

(defmethod emit :map [node backend]
  (throw (ex-info "Map literals unsupported in shader" {:node node})))

(defmethod emit :convert [node backend]
  (sp/shader-cast backend
                  (emit (:expr node) backend)
                  (:src-type (:attrs node))
                  (:dst-type (:attrs node))))

(defmethod emit :default [node backend]
  (throw (ex-info (str "Unhandled node: " node) {:node node})))

;; 简单 generate：只处理函数定义和资源定义，不生成入口包装和结构体
(defn generate [ir2-roots backend entry-stage entry-fn-name]
  (let [defines       (filter #(= (ir2p/kind %) :define) ir2-roots)
        resource-defs (filter #(:resource-type (ir2p/node-meta %)) defines)
        function-defs (remove #(:resource-type (ir2p/node-meta %)) defines)
        fn-defs       (map #(emit % backend) function-defs)
        globals       (map #(emit % backend) resource-defs)
        others        (remove #(= (ir2p/kind %) :define) ir2-roots)]
    (if (seq function-defs)
      ;; 有函数定义：输出函数定义和入口包装
      (let [main-fn      (first function-defs)
            fn-name      (sp/shader-var-ref backend (:name main-fn))
            entry        (sp/shader-entry-point backend entry-stage fn-name)]
        (str (str/join "\n" globals)
             "\n"
             (str/join "\n" fn-defs)
             "\n"
             entry))
      ;; 无函数定义：为非定义表达式生成入口
      (let [other-code  (when (seq others)
                          (let [exprs (map #(emit % backend) others)]
                            (if (= 1 (count exprs))
                              (sp/shader-return backend (first exprs))
                              (sp/shader-block backend exprs))))
            entry       (sp/shader-entry-point backend entry-stage entry-fn-name)]
        (str (str/join "\n" globals)
             "\n"
             other-code
             "\n"
             entry)))))