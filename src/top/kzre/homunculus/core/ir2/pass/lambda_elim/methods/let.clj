(ns top.kzre.homunculus.core.ir2.pass.lambda-elim.methods.let
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.pass.lambda-elim.core :as elim]
            [top.kzre.homunculus.core.ir2.pass.lambda-elim.env :as p]))

;; TODO, 目前还不支持 lambda 作为一等公民，虽然能在调用点展开，但仍不支持lambda 赋值给变量，随变量传递
;; 后续拓展必须
;; 1. 如果 lambda 被绑定变量，先提升
;; 2. 然后替换词法作用域内的符号,并随着赋值传递, 这个有些超出我们静态分析的范畴了
(defmethod elim/elim-node* :let [node env]
  (let [bindings (n/let-bindings node)         ;; Binding 向量
        ;; 值表达式在外部环境下处理
        [new-bindings binding-defs] (elim/elim-seq bindings env)
        ;; 扩展内部环境——绑定名可见于 body
        binding-names (map #(:name (:var %)) bindings)
        inner-env     (reduce p/bind-var env binding-names)
        [new-body body-defs] (elim/elim-node* (n/let-body node) inner-env)
        new-let (n/make-let new-bindings new-body
                            (n/attrs node) (n/node-meta node))]
    [new-let
     (into binding-defs body-defs)]))