(ns top.kzre.homunculus.core.types.check.methods.array
  "数组特殊节点的双向检查。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :new-array [node expected context]
  ;; new-array 自身不产生值，它的类型已在约束求解中确定，无需进一步检查。
  ;; 只检查 size 子节点的类型（应为整数）。
  (let [size (n/new-array-size node)
        int-ty (ty/make-tcon (check/integer-type context))]
    (n/make-new-array (check/check-node size int-ty context)
                      (n/node-meta node)
                      (n/parent node))))

(defmethod check/check-node :aget [node expected context]
  (let [target (n/aget-target node)
        idx (n/aget-idx node)
        int-ty (ty/make-tcon (check/integer-type context))
        ;; 索引必须是整数
        checked-idx (check/check-node idx int-ty context)
        ;; target 的类型已由约束确定，无需额外期望类型，但可以传递 nil 或使用其已知类型
        checked-target (check/check-node target nil context)
        ;; aget 的返回类型已由约束求解标记在节点上，expected 可用来验证或插入转换
        new-node (n/make-aget checked-target checked-idx
                              (n/node-meta node) (n/parent node))
        actual (ty/get-type new-node (check/known-types context))]
    (if (and expected actual)
      (check/check-type new-node expected context)
      new-node)))

(defmethod check/check-node :aset [node expected context]
  (let [target (n/aset-target node)
        idx (n/aset-idx node)
        val (n/aset-val node)
        int-ty (ty/make-tcon (check/integer-type context))
        checked-idx (check/check-node idx int-ty context)
        ;; 目标数组的类型已确定，元素类型可从其类型获取
        target-ty (ty/get-type (n/aset-target node) (check/known-types context))
        elem-ty (when (ty/vec-type? target-ty) (ty/vec-element-type target-ty))]
    (n/make-aset (check/check-node target nil context)
                 checked-idx
                 (if elem-ty
                   (check/check-node val elem-ty context)   ; 期望值类型匹配数组元素类型
                   (check/check-node val nil context))
                 (n/node-meta node)
                 (n/parent node))))

(defmethod check/check-node :alength [node expected context]
  (let [target (n/alength-target node)
        int-ty (ty/make-tcon (check/integer-type context))]
    (n/make-alength (check/check-node target nil context)
                    (n/node-meta node)
                    (n/parent node))))
