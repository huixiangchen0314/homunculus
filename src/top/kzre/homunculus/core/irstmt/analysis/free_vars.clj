(ns top.kzre.homunculus.core.irstmt.analysis.free-vars
  "分析函数的自由变量——函数中 use 但不在函数内 def 的变量。

   free-vars = #{var-name}——只需要变量名。

   strip 之后 assign 的 lhs 保留完整 attrs——polyfill 需要
   attrs 时从 assign 的 lhs 拿。free-vars 只用于过滤——标识
   哪些变量是全局的（不声明）。

   排除参数——参数由函数签名声明。"
  (:require
    [clojure.set :as set]
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

(defn- walk
  "遍历节点，收集 def 和 use。

   defs —— 函数内 def 的变量名集合（assign lhs、var-decl name）
   uses —— 函数内 use 的变量名集合 #{var-name}

   返回 [defs' uses']。

   ▸ :assign    —— lhs 是 def，只递归 rhs
   ▸ :var-decl  —— name 是 def，val 可选
   ▸ :variable  —— 收集 use
   ▸ 其他       —— ast/children 递归——覆盖 block / if / while /
                   throw / 表达式节点的所有子节点"
  [node defs uses]
  (case (ast/kind node)
    :assign
    (let [lhs   (:var node)
          defs' (if (= :variable (ast/kind lhs))
                  (conj defs (:name lhs))
                  defs)]
      (walk (:val node) defs' uses))

    :var-decl
    (let [defs' (conj defs (:name node))]
      (if-let [val (:val node)]
        (walk val defs' uses)
        [defs' uses]))

    :variable
    [defs (conj uses (:name node))]

    (reduce (fn [[d u] child] (walk child d u))
            [defs uses]
            (ast/children node))))

(defn analyze-function
  "分析 function/method 节点的自由变量。

   返回 #{var-name}——函数中 use 但不在函数内 def 的变量
   （排除参数）。"
  [fn-node]
  (let [params      (set (map :name (:params fn-node)))
        [defs uses] (walk (:body fn-node) #{} #{})
        local-defs  (into defs params)]
    (set/difference uses local-defs)))