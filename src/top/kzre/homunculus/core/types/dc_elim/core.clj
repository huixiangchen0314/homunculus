(ns top.kzre.homunculus.core.types.dc-elim.core
  "死代码消除 Pass：根据配置消除未引用的高阶函数定义。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]))

(defn eliminate-ho-defs
  "从 IR2 根列表中移除所有标记为 :ho? true 的 define 节点。
   其余节点（资源、记录、入口函数等）保留。
   当配置 :dce-eliminate-ho? 为 false 时，不做任何过滤。"
  [ir2-roots context]
  (if (get context :dce-eliminate-ho? true)
    (filterv (fn [root]
               (if (n/define-node? root)
                 (not (-> root n/attrs :ho?))
                 true))
             ir2-roots)
    ir2-roots))