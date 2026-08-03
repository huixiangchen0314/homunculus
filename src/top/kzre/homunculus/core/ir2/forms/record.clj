(ns top.kzre.homunculus.core.ir2.forms.record
  "defrecord 的 IR2 lowering：使用显式节点构建完整 Record。"
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :record [node env]
  (let [name      (n1/record-name node)
        ir1-fields    (n1/record-fields node)      ;; IR1 字段描述向量 (map)
        ir1-protocols (n1/record-protocols node)   ;; IR1 ProtocolImpl 向量
        meta      (n1/node-meta node)

        ;; 1. 字段 lowering：生成 IR2 Field 节点
        ir2-fields (mapv (fn [f]
                           (n2/make-field (:name f) {} (:meta f)))
                         ir1-fields)

        ;; 2. 协议实现 lowering
        ir2-protocols (mapv (fn [proto-impl]
                              (let [proto-name (:proto-name proto-impl)
                                    ir1-methods (:methods proto-impl)]
                                (n2/make-protocol-impl
                                  proto-name
                                  (mapv (fn [m]
                                          ;; 参数 lowering：IR1 Param -> IR2 Param
                                          (let [params (mapv (fn [p]
                                                               (n2/make-param (:name p) {} (:meta p)))
                                                             (:params m))
                                                ;; 方法体 lowering
                                                [body _] (ir2/lower-ast (:body m) env)]
                                            (n2/make-method (:name m)
                                                            params
                                                            body
                                                            (:doc m)
                                                            {}          ; attrs
                                                            (:meta m))))
                                        ir1-methods)
                                  {}
                                  (:meta proto-impl))))
                            ir1-protocols)
        ;; 3. 构建 IR2 Record 节点
        record-node (n2/make-record name ir2-fields ir2-protocols {} meta)]
    [record-node]))