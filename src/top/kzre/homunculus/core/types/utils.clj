(ns top.kzre.homunculus.core.types.utils
  (:require [top.kzre.homunculus.core.ir2.protocol :as p]))


(defn fresh-name [base]
  (symbol (str (name base) "_" (gensym ""))))


(defn find-toplevel-define
  "在 IR2 根节点列表中查找指定名称的顶层函数定义（:define 节点）。

   参数：
     ir2-roots — 一个 IR2 根节点的序列（通常是整个编译单元的顶层表达式列表）。
     name      — 要查找的函数名称，通常为符号（如 'my-func）。

   返回值：
     第一个满足以下条件的根节点：
       - 满足 ir2p/INode 协议（即复合节点）；
       - 其 :kind 为 :define；
       - 其 :name 等于给定的 name。
     如果找不到匹配的定义，则返回 nil。

   说明：
     - 该函数使用 `some` 遍历序列，效率为 O(n)，适用于查找少量顶层定义。
     - 仅检查顶层节点，不会递归进入定义体内部。
     - 假设顶层定义名称是唯一的，若有重名，只返回最先找到的那一个。
     - 常用于闭包提升或单态化过程中，判断某个变量引用是否指向已知的顶层函数，
       以便决定是否可以进行进一步的特化或替换。

   示例：
     ;; 查找名为 'add 的顶层函数定义
     (find-toplevel-define program-roots 'add)"
  [ir2-roots name]
  (some (fn [root]
          (when (and (satisfies? p/INode root)
                     (= (p/kind root) :define)
                     (= (:name root) name))
            root))
        ir2-roots))


