(ns top.kzre.homunculus.core.ir1.forms.member-access
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

;; 关键字访问
(defmethod ir1/form->node :keyword-access [form]
  (let [[k obj] form]
    (n/make-member-access obj k nil (meta form))))

;; 方法调用：(. obj method args...)
(defmethod ir1/form->node '. [form]
  (let [[_ obj method & args] form]
    (n/make-member-access obj method (into [obj] args) (meta form))))

(defmethod ir1/build-tree :member-access [node]
  (let [target (n/member-access-target node)
        member (n/member-access-accessor node)
        args   (n/member-access-args node)
        new-target (ir1/->ir1 target)
        new-args   (mapv ir1/->ir1 args)]
    (n/make-member-access new-target member new-args
                          (n/node-meta node)
                          (n/parent node))))