(ns top.kzre.homunculus.core.ir1.forms.record
  (:require
    [top.kzre.homunculus.core.ir1.core :as ir1]
    [top.kzre.homunculus.core.ir1.ast :as m]
    [top.kzre.homunculus.core.ir1.node :as n]))

;; 字段名是结构体成员属性，不 qualified，保持裸符号。
(defn- parse-fields [field-vec]
  (->> field-vec
       (filter symbol?)
       (mapv (fn [sym] (m/->Field sym (meta sym))))))

;; 方法名是协议方法短名，属于 proto-name，不单独 qualified。
(defn- parse-method [method-form env]
  (let [[method-name params-vec & body-exprs] method-form
        docstring   (when (string? (first body-exprs)) (first body-exprs))
        body-forms  (if docstring (rest body-exprs) body-exprs)
        method-meta (merge (meta method-name) (meta method-form) (meta params-vec))
        params      (rest params-vec)
        env-params  (reduce (fn [e p] (ir1/bind-local-symbol e p)) env params)
        param-nodes (mapv (fn [p] (m/->Param p (meta p))) params)
        [body-nodes _] (ir1/lower-seq body-forms env-params)
        ir-body     (n/wrap-body body-nodes (meta body-forms))]
    (m/->Method method-name param-nodes ir-body docstring method-meta)))

;; 协议名是对外部协议的引用，走 qualify-symbol（别名 / :refer / 本 ns 均可）。
(defn- parse-protocols [body-forms env]
  (loop [forms body-forms
         current-proto nil
         result []]
    (if-let [form (first forms)]
      (if (symbol? form)
        (let [proto-sym (ir1/qualify-symbol env form)]           ; ← 新增
          (recur (rest forms)
                 {:proto-name proto-sym :methods []}
                 (if current-proto (conj result current-proto) result)))
        (let [method (parse-method form env)]
          (if current-proto
            (recur (rest forms)
                   (update current-proto :methods conj method)
                   result)
            (throw (ex-info "Method without protocol" {:method form})))))
      (if current-proto (conj result current-proto) result))))

(defmethod ir1/lower-node* 'defrecord [form env]
  (let [[_ record-sym field-vec & body-forms] form
        ns-part       (name (ir1/ns-sym env))
        qualify       #(with-meta (ir1/qualify-ns-symbol env %) (meta record-sym))
        record-name   (qualify record-sym)
        ctor-name     (symbol ns-part (str "->" (name record-sym)))
        env'          (-> env
                          (ir1/bind-global-symbol record-name)
                          (ir1/bind-global-symbol ctor-name))
        fields         (parse-fields field-vec)
        proto-maps     (parse-protocols body-forms env')
        protocol-impls (mapv (fn [pm]
                               (m/->ProtocolImpl (:proto-name pm)
                                                 (:methods pm)
                                                 (meta form)))
                             proto-maps)]
    [(m/->Record record-name fields protocol-impls (meta form)) env']))