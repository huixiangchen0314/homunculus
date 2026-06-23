(ns top.kzre.homunculus.core.types.module.collect-symbols
  "收集命名空间符号, 注册到编译上下文。
   每次调用都尽可能收集完整信息：声明、类型、元数据、高阶标记及 IR 子树。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.internal.symbol :as sym]
    [top.kzre.homunculus.internal.protocol :as p]))

(defn- fully-qualified-sym [node]
  (n/define-name node))

(defn- collect-define [node context]
  (let [s   (fully-qualified-sym node)
        val (n/define-val node)]
    (if (= :lambda (n/kind val))
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
                                  :meta (n/node-meta val))]
        (cond-> entry
                (ty/get-type val) (assoc :type (ty/get-type val))
                (true? (:ho? (n/attrs node))) (assoc :ho? true :ir2 val)))
      ;; 变量
      (sym/make-variable s
                         :type (ty/get-type node)
                         :meta (n/node-meta node)))))

(defn- collect-record [node context]
  (let [s        (n/record-name node)
        fields   (mapv (fn [f]
                         (sym/make-field (n/field-name f)
                                         :type (ty/get-type f)
                                         :meta (n/field-meta f)))
                       (n/record-fields node))
        protocols (n/record-protocols node)
        entry    (sym/make-record s :fields fields :protocols protocols
                                  :type (ty/get-type node)
                                  :meta (n/node-meta node))]
    entry))

(defn- collect-protocol [node context]
  (let [s       (n/protocol-name node)
        methods (mapv (fn [method-desc]
                        (let [mname   (n/method-name method-desc)
                              params  (mapv (fn [p]
                                              (sym/make-param (:name p)
                                                              :type (:type p)
                                                              :meta (:meta p)))
                                            (n/method-params method-desc))
                              ret     (sym/make-ret (n/method-ret method-desc)
                                                    :meta (n/method-meta method-desc))]
                          (sym/make-method mname
                                           [(sym/make-func-arity params :ret ret)])))
                      (n/protocol-methods node))
        entry   (sym/make-protocol s :methods methods
                                   :type (ty/get-type node)
                                   :meta (n/node-meta node))]
    entry))

(defn collect-symbols
  "遍历 IR2 根节点，收集所有顶层定义并注册到 context。"
  [ir2-roots context]
  (doseq [root ir2-roots]
    (try
      (let [entry (case (n/kind root)
                    :define   (collect-define root context)
                    :record   (collect-record root context)
                    :protocol (collect-protocol root context)
                    nil)]
        (when entry
          (p/register-sym context entry)))
      (catch Throwable t
        (println "[WARN] collect-symbols failed for" (n/kind root) ":" (.getMessage t))))))