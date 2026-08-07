(ns top.kzre.homunculus.core.types.module.collect-symbols
  "收集命名空间符号, 注册到编译上下文。
   每次调用都尽可能收集完整信息：声明、类型、元数据、高阶标记及 IR 子树。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.internal.symbol :as sym]
    [top.kzre.homunculus.internal.module-unit]
    [top.kzre.homunculus.internal.protocol :as p]
    [top.kzre.homunculus.internal.module-unit :as mu])
  (:import (top.kzre.homunculus.internal.module_unit ModuleUnit)))

(defn- fully-qualified-sym [node]
  (n/define-name node))

(defn- collect-define [node context]
  (let [s   (fully-qualified-sym node)
        val (n/define-val node)]
    (cond
      (= :lambda (n/kind val))
      ;; 函数
      (let [params (mapv (fn [p]
                           (sym/make-param (n/var-name p)
                                           :type (ty/get-type p)
                                           :meta (n/node-meta p)))
                         (n/lambda-params val))
            ret    (when-let [body (n/lambda-body val)]
                     (sym/make-ret (ty/get-type body)
                                   :meta (n/node-meta body)))
            entry  (sym/make-func s
                                  :params params
                                  :ret ret
                                  :type (ty/get-type val)
                                  :meta (n/node-meta val))
            attrs (n/attrs node)
            flags (->> (select-keys attrs [:ho? :inline :polymorphic])
                       (filter (fn [[_ v]] (true? v)))
                       (into {}))]
        (cond-> entry
                (seq flags)
                (merge flags {:ir2 val})))
      :else
      ;; 变量
      (sym/make-variable s
                         :type (ty/get-type node)
                         :meta (n/node-meta node)))))

(defn- collect-record [node context]
  (let [s              (n/record-name node)                ; 记录名符号
        fields-vec     (:fields node)                     ; Field 节点向量
        protocol-impls (:protocols node)                  ; ProtocolImpl 向量

        ;; 收集字段信息
        fields (mapv (fn [f]
                       (sym/make-field (:name f)
                                       :type (ty/get-type f)
                                       :meta (:meta f)))
                     fields-vec)

        protocols (mapv (fn [impl]
                          {:protocol-name (:proto-name impl)
                           :impl-method-names (mapv :name (:methods impl))})
                        protocol-impls)

        record-entry (sym/make-record s
                                      :fields fields
                                      :protocols protocols
                                      :meta (n/node-meta node))

        ;; 生成构造器条目 ->RecordName
        ctor-name   (symbol (str "->" (name s)))
        field-tys   (mapv :type fields)                   ; 从字段条目中取类型
        record-ty   (ty/make-tcon s)
        ctor-type   (reduce (fn [ret arg] (ty/make-tfun arg ret))
                            record-ty
                            (reverse field-tys))
        ctor-params (mapv (fn [f]
                            (sym/make-param (get f :field-name)   ; sym/make-field 返回包含 :field-name
                                            :type (:type f)
                                            :meta (:meta f)))
                          fields)
        ctor-entry  (sym/make-func ctor-name
                                   :params ctor-params
                                   :ret (sym/make-ret record-ty)
                                   :type ctor-type
                                   :meta {})]
    (p/register-sym context record-entry)
    (p/register-sym context ctor-entry)
    record-entry))

(defn emit-record-ctor [entry ^ModuleUnit unit]
  (let [sym (mu/norm-sym unit (:sym entry))
        record-ty   (ty/make-tcon sym)
        fields     (:fields entry)
        field-tys   (mapv :type fields)
        ctor-name   (mu/norm-sym unit (symbol (str "->" (name sym))))
        ctor-type   (reduce (fn [ret arg] (ty/make-tfun arg ret))
                            record-ty
                            (reverse field-tys))
        ctor-params (mapv (fn [f]
                            (sym/make-param (get f :field-name)   ; sym/make-field 返回包含 :field-name
                                            :type (:type f)
                                            :meta (:meta f)))
                          fields)]
    (sym/make-func ctor-name
                   :params ctor-params
                   :ret (sym/make-ret record-ty)
                   :type ctor-type
                   :meta {:cljh/ctor? true                  ;; 自动生成的构造器
                          })))

(defn- collect-protocol [node _context]
  (let [proto-name       (n/protocol-name node)
        methods (mapv (fn [method-node]
                        (let [mname   (n/method-name method-node)
                              params  (mapv (fn [param-node]
                                              (sym/make-param (:name param-node)
                                                              :type (ty/get-type param-node)
                                                              :meta (:meta param-node)))
                                            (n/method-params method-node))
                              ret     (sym/make-ret  (ty/get-type method-node)
                                                    :meta (n/method-meta method-node))]
                          (sym/make-method mname
                                           [(sym/make-func-arity params :ret ret)])))
                      (n/protocol-methods node))
        entry   (sym/make-protocol proto-name
                                   :methods methods
                                   :meta (n/node-meta node))]
    entry))

(defn collect-symbols
  "遍历 IR2 根节点，收集所有顶层定义并注册到 context。"
  [ir2-roots context ^ModuleUnit unit]
  (let [module-atom (atom unit)]
    (doseq [root ir2-roots]
      (try
        (case (n/kind root)
          :define   (when-let [entry (collect-define root context)]
                      (swap! module-atom mu/register-symbol entry)
                      (p/register-sym context entry))
          :record   (when-let [entry (collect-record root context)]
                      (swap! module-atom mu/register-symbol entry)
                      (let [ctor-entry (emit-record-ctor entry unit)]
                        (swap! module-atom mu/register-symbol ctor-entry)))
          :protocol (when-let [entry (collect-protocol root context)]
                      (swap! module-atom mu/register-symbol entry)
                      (p/register-sym context entry))
          nil)
        (catch Throwable t
          (println "[WARN] collect-symbols failed for" (n/kind root) ":" (.getMessage t)))))
    @module-atom))