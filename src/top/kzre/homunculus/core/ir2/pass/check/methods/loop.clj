(ns top.kzre.homunculus.core.ir2.pass.check.methods.loop
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod check/check-node* :loop [node expected env]
  (let [bindings         (n/loop-bindings node)
        checked-bindings (mapv (fn [binding]
                                 (let [new-var (check/check-node* (:var binding) nil env)
                                       new-val (check/check-node* (:val binding) nil env)]
                                   (assoc binding :var new-var :val new-val)))
                               bindings)
        ;; 把循环变量绑进 env，供 body 和 recur 查询
        body-env   (reduce (fn [current-env binding]
                             (let [var-node (:var binding)
                                   var-name (:name var-node)
                                   var-type (ty/get-type var-node)]
                               (if var-type
                                 (p/bind-var current-env var-name var-type)
                                 current-env)))
                           env
                           checked-bindings)
        ;; 标记当前循环的变量名列表
        body-env   (p/with-loop-vars body-env
                                     (mapv #(:name (:var %)) checked-bindings))
        body-node  (check/check-node* (n/loop-body node) nil body-env)]
    (n/make-loop checked-bindings body-node
                 (n/attrs node)
                 (n/node-meta node))))

(defmethod check/check-node* :recur [node expected env]
  (let [loop-names (p/loop-vars env)]
    (when-not loop-names
      (throw (ex-info "recur outside loop" {})))
    (let [args (:args node)]
      (when (not= (count args) (count loop-names))
        (throw (ex-info "recur arg count mismatch" {})))
      (let [checked-args (mapv (fn [arg var-name]
                                 (let [expected-type (p/resolve-var-type env var-name)]
                                   (check/check-node* arg expected-type env)))
                               args loop-names)]
        (n/make-recur checked-args
                      (n/attrs node)
                      (n/node-meta node))))))