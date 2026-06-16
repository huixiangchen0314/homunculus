(ns top.kzre.homunculus.core.ir2.forms.record
  "defrecord 的 IR2 lowering：分离类型声明与方法实现。"
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :record [node env]
  (let [name      (n1/record-name node)
        fields    (n1/record-fields node)      ;; 字段描述向量
        protocols (n1/record-protocols node)   ;; 协议实现向量
        meta      (n1/node-meta node)

        ;; lowering 字段默认值
        new-fields (mapv (fn [f]
                           (if-let [init (n1/field-init f)]
                             (n1/field-with-init f (first (ir2/lower-ast init env)))
                             f))
                         fields)

        ;; 构建 RecordNode（声明）
        record-node (n2/make-record name new-fields
                                    (mapv :protocol protocols)   ;; 仅保留协议名列表
                                    {} meta nil)

        ;; 为每个方法生成 DefineNode（函数实现）
        method-defs (mapcat
                      (fn [proto]
                        (map (fn [m]
                               (let [method-name (n1/arity-name m)
                                     params      (n1/arity-params m)   ;; 参数描述向量（含 this）
                                     body        (n1/arity-body m)     ;; IR1 节点
                                     ;; 构建函数名：record-name-method-name
                                     fn-name     (symbol (str name "-" method-name))
                                     ;; lowering 参数：每个参数描述中的 :name 是符号，直接生成 VariableNode
                                     ir2-params  (mapv (fn [p]
                                                         (let [sym (n1/param-sym p)]
                                                           (n2/make-variable (name sym) {} (meta sym) nil)))
                                                       params)
                                     ;; lowering 方法体
                                     ir2-body    (first (ir2/lower-ast body env))
                                     ;; 构造 LambdaNode
                                     lambda-node (n2/make-lambda ir2-params ir2-body [] nil
                                                                 {} nil nil)
                                     ;; 定义节点
                                     ]
                                 (n2/make-define fn-name lambda-node nil {} {} nil)))
                             (:methods proto)))
                      protocols)]

    (into [record-node] method-defs)))