(ns top.kzre.homunculus.backend.shader.pass.mark-escape.core
  "标记逃逸的数组变量。

   逃逸规则（当前）：
   1. 函数/入口点的 :return-type 是数组类型时，检查 body 的 :ret 表达式；
   2. 若 :ret 直接是变量引用，则标记该变量逃逸；
   3. 其他情况（构造 / 调用 / 复合表达式）暂不处理。

   标记写入 :attrs/escape?。

   TODO：
   - 返回表达式为构造 / 调用时，递归追踪其数据来源；
   - 数组作为 :call 参数传给不纯函数；
   - 数组被 :assign 到逃逸位置（uniform / static-var）；
   - 数组在 :ret 之外的全局副作用路径。"
  (:require
    [top.kzre.homunculus.backend.shader.ast :as ast]
    [top.kzre.homunculus.backend.shader.pass.mark-escape.env :as env]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))


(defn- mark-node
  "递归遍历 node，把与 env 中 return-var 同名的 :var-decl 标记逃逸。"
  [node env]
  (let [[new-node _]
        (ast/reduce-children
          node
          (fn [child e] [(mark-node child e) e])
          env)]
    (if (and (= :var-decl (ast/kind new-node))
             (env/return-var? env (:name new-node)))
      (update new-node :attrs assoc :escape? true)
      new-node)))


(defn- mark-function
  "处理单个函数节点。"
  [node env]
  (let [ret-type (:return-type node)
        ret-expr (get-in node [:body :ret])]
    (if (and (ty/vec-type? ret-type)
             (some? ret-expr)
             (= :variable (ast/kind ret-expr)))
      (let [var-name (:name ret-expr)
            env'     (env/set-return-var env var-name)]
        (update node :body mark-node env'))
      node)))


(defn mark
  "顶层入口：只处理 :function / :entry-point，其他节点原样透传。"
  [nodes env]
  (mapv (fn [node]
          (case (ast/kind node)
            (:function :entry-point) (mark-function node env)
            node))
        nodes))