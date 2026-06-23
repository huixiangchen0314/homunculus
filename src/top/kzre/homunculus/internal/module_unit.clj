;; top.kzre.homunculus.internal.module_unit.clj
(ns top.kzre.homunculus.internal.module-unit
  "模块编译单元：保存单个命名空间的编译中间结果。")

(defrecord ModuleUnit
  [ns-sym          ;; 命名空间符号
   ir2-roots       ;; 经过约束求解（solve）之后的 IR2 根节点列表
   ])