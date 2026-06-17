(ns top.kzre.homunculus.core.types.utils
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.protocol :as tp]))


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
          (when (and (satisfies? ir2p/INode root)
                     (= (ir2p/kind root) :define)
                     (= (:name root) name))
            root))
        ir2-roots))



(defn known-fn-name?
  "判断给定的 IR2 节点是否为一个指向已知顶层函数定义的变量引用。

   参数：
     ir2-roots — 当前编译单元的顶层表达式列表（即根节点集合），用于查找已有的顶层 define。
     node      — 待判断的 IR2 节点。

   返回值：
     如果 node 是一个 :variable 节点，且其 :name 对应的符号能在 ir2-roots 中找到相应的顶层 :define 定义，
     则返回该 define 节点（真值）；否则返回 nil。

   说明：
     - 该函数用于在闭包消除的上下文收集阶段，判断一个调用节点的 :fn 部分是否引用了某个已知的顶层函数。
       如果是，则调用中出现在该函数参数位置的 lambda 可以被特殊处理（如进行单态化）。
     - 函数内部通过将节点名称转换为 symbol 再调用 u/find-toplevel-define 进行查找，
       确保匹配的是符号名称而非其他类型（如字符串）。
     - 这是一个私有函数（defn-），仅在当前命名空间内使用。

   示例：
     ;; 假设顶层定义了 (define f (lambda (x) ...))
     ;; 则对于表示 'f 的 :variable 节点，调用 (known-fn-name? roots f-node) 将返回定义节点；
     ;; 对于未定义的变量 'g，返回 nil。"
  [ir2-roots node]
  (and (satisfies? ir2p/INode node)
       (= (ir2p/kind node) :variable)
       (find-toplevel-define ir2-roots (symbol (:name node)))))