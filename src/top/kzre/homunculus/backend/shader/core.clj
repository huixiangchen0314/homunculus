(ns top.kzre.homunculus.backend.shader.core
  "着色器后端通用辅助函数。提供资源分类、参数提取、入口规格构建等纯数据逻辑，
   不依赖任何具体着色语言。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.core.types.metadata :as md]))

;; ── 语义提取 ────────────────────────────
(defn extract-semantic
  "从参数节点的元数据中提取语义字符串，如 POSITION → \"POSITION\"。
   只取无命名空间的大写关键字。"
  [param-node]
  (some (fn [k]
          (when (and (keyword? k)
                     (not (namespace k))
                     (re-find #"^[A-Z]" (name k)))
            (name k)))
        (keys (md/node-meta param-node))))

;; ── 参数信息提取 ────────────────────────
(defn extract-params
  "将函数参数列表转换为向量，每个元素为 {:name :type :semantic}。
   type 通过 shader.types/shader-type-str 转换为字符串（由调用者提供类型转换函数）。
   此函数只负责提取 IR 数据，类型转换函数作为参数传入。"
  [params type-conv-fn]
  (mapv (fn [p]
          {:name     (name (n/var-name p))
           :type     (type-conv-fn (ty/get-type p))
           :semantic (extract-semantic p)})
        params))

;; ── 定义分类 ────────────────────────────
(defn classify-defines
  "将 :define 节点分为资源、全局常量、uniform 和函数。
   - 资源：attrs 中有 :shader/resource? 标记
   - uniform：attrs 中有 :shader/uniform? 标记
   - 函数：val 是 lambda 节点
   - 全局常量：其余"
  [defines]
  (let [resource? #(some-> (n/node-meta %) :shader/resource?)
        uniform?  #(some-> (n/node-meta %) :shader/uniform?)
        function? #(when-let [v (n/define-val %)]
                     (= (n/kind v) :lambda))]
    {:resources (filter resource? defines)
     :uniforms  (filter uniform? defines)
     :globals   (remove #(or (resource? %) (uniform? %) (function? %)) defines)
     :functions (filter function? defines)}))

;; ── 入口规格构建 ────────────────────────
(defn entry-spec
  "为给定的着色器入口函数构建入口规格 map。
   stage :vertex / :fragment 等
   define-node :define 节点，其 val 为 lambda
   type-conv-fn : 类型转换函数 (IType -> string)
   返回 {:input-params [...] :output-params [...]}，
   其中 input/output params 均为 {:name :type :semantic} 向量。"
  [stage define-node type-conv-fn]
  (let [lam        (n/define-val define-node)
        params     (n/lambda-params lam)
        raw-inputs (extract-params params type-conv-fn)
        ;; 片段着色器可过滤掉无语义的输入（如果不需要）
        input-params (if (= stage :fragment)
                       (filterv (comp some? :semantic) raw-inputs)
                       raw-inputs)
        ;; 返回类型
        ret-type-str (type-conv-fn (ty/get-type lam))
        output-param {:name     "pos"
                      :type     ret-type-str
                      :semantic (case stage
                                  :vertex   "SV_POSITION"
                                  :fragment "SV_TARGET"
                                  :geometry "SV_POSITION"
                                  nil)}]
    {:input-params  input-params
     :output-params [output-param]}))