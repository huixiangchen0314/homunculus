(ns top.kzre.homunculus.core.types.free-vars
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defn- collect-bound
  "纯函数：遍历 node 子树，返回所有局部绑定的变量名集合。
   将 :let、:loop、:lambda 引入的绑定名收集到集合中，并通过递归合并子节点结果。"
  [node]
  (if (satisfies? ir2p/INode node)
    (let [here (case (ir2p/kind node)
                 :let    (into #{} (map (comp :name first) (:bindings node)))
                 :lambda (into #{} (map :name (:params node)))
                 :loop   (into #{} (map (comp :name first) (:bindings node)))
                 #{})
          ;; 递归所有子节点，将结果合并
          children-bounds (reduce into #{} (map collect-bound (ir2p/children node)))]
      (into here children-bounds))
    #{}))

(defn- collect-free
  "纯函数：给定已收集的 bound 集合，遍历 node 子树，返回所有未被绑定的变量引用集合。
   遇到 :variable 节点时，若其名称不在 bound 中则加入结果；否则继续递归子节点。"
  [bound node]
  (if (satisfies? ir2p/INode node)
    (if (= (ir2p/kind node) :variable)
      (if (contains? bound (:name node))
        #{}
        #{(:name node)})
      ;; 非变量节点：合并所有子节点的自由变量
      (reduce into #{} (map (partial collect-free bound) (ir2p/children node))))
    #{}))

(defn analyze
  "对给定的 AST 节点 node 进行自由变量分析，返回一个集合，包含所有在 node 子树中
   被引用但未在其内部任何局部绑定中定义的变量名。

   算法：
   1. 纯函数 collect-bound 收集子树内所有局部绑定名。
   2. 纯函数 collect-free 利用该集合找出所有自由变量。
   3. 返回最终的自由变量集合。

   注意：本函数不感知外层作用域（如 lambda 的形参），调用者如需进一步过滤，
   应自行与外部已知绑定做差集（参见 free-vars 函数）。"
  [node]
  (let [bound (collect-bound node)
        _     (println "free-vars: bound =" bound)
        free  (collect-free bound node)]
    (println "free-vars: result =" free)
    free))