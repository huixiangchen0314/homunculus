(ns top.kzre.homunculus.core.ir2.pass.check.methods.array
  "数组特殊节点的双向检查。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod check/check-node* :new-array [node expected env]
  ;; new-array 自身不产生值，它的类型已在约束求解中确定，无需进一步检查。
  ;; 只检查 size 子节点的类型（应为整数）。
  (let [size   (n/new-array-size node)
        int-ty (ty/make-tcon (p/integer-type env))]
    (assoc node
      :size (check/check-node* size int-ty env))))

(defmethod check/check-node* :aget [node expected env]
  (let [target  (n/aget-target node)
        idx     (n/aget-idx node)
        int-ty  (ty/make-tcon (p/integer-type env))
        ;; 索引必须是整数
        checked-idx    (check/check-node* idx int-ty env)
        ;; target 的类型已由约束确定，无需额外期望类型
        checked-target (check/check-node* target nil env)
        ;; aget 的返回类型已由约束求解标记在节点上，expected 可用来验证或插入转换
        new-node (n/make-aget checked-target checked-idx
                              (n/node-meta node))
        actual   (ty/get-type new-node)]
    (if (and expected actual)
      (check/check-type new-node expected env)
      new-node)))

(defmethod check/check-node* :aset [node expected env]
  (let [target  (n/aset-target node)
        idx     (n/aset-idx node)
        val     (n/aset-val node)
        int-ty  (ty/make-tcon (p/integer-type env))
        checked-idx (check/check-node* idx int-ty env)
        ;; 目标数组的类型已确定，元素类型可从其类型获取
        target-type (ty/get-type (n/aset-target node))
        elem-type   (when (ty/vec-type? target-type)
                      (ty/vec-element-type target-type))]
    (n/make-aset (check/check-node* target nil env)
                 checked-idx
                 (if elem-type
                   (check/check-node* val elem-type env)
                   (check/check-node* val nil env))
                 (n/node-meta node))))

(defmethod check/check-node* :alength [node expected env]
  (let [target (n/alength-target node)]
    (n/make-alength (check/check-node* target nil env)
                    (n/node-meta node))))