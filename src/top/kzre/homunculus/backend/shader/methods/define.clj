(ns top.kzre.homunculus.backend.shader.methods.define
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]
            [top.kzre.homunculus.backend.shader.protocol :as sp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [clojure.string :as str]))

(defmethod emit :define [node backend]
  (let [val (:val node)
        val-ty (get-in val [:attrs :type])]
    (if (and val-ty
             (satisfies? tp/IType val-ty)
             (= (tp/type-kind val-ty) :con)
             (contains? #{:texture2D :sampler :cbuffer} (:name val-ty)))
      ;; 资源声明
      (sp/shader-resource-decl backend
                               (sp/shader-var-ref backend (:name node))
                               (:name val-ty)
                               (:args val))
      ;; 普通函数定义
      (let [params (:params val)
            body   (:body val)
            param-strs (map (fn [p]
                              (let [ty (get-in p [:attrs :type])
                                    type-str (if ty (sp/shader-type backend ty) "float")
                                    param-name (sp/shader-var-ref backend (:name p))
                                    metadata (ir2p/node-meta p)
                                    semantic (when (map? metadata)
                                               (some (fn [k]
                                                       (when (and (keyword? k)
                                                                  (not (namespace k))
                                                                  (re-find #"^[A-Z]" (name k)))
                                                         k))
                                                     (keys metadata)))
                                    modifier (cond
                                               (contains? (set (keys metadata)) :out)   "out"
                                               (contains? (set (keys metadata)) :inout) "inout"
                                               :else nil)]
                                (str (when modifier (str modifier " "))
                                     type-str " " param-name
                                     (if semantic (str " : " (name semantic)) ""))))
                            params)
            body-code (emit body backend)
            return-type (if-let [rt (get-in body [:attrs :type])]
                          (if (and (satisfies? tp/IType rt) (= (tp/type-kind rt) :con) (= (:name rt) :nil))
                            "void"
                            (sp/shader-type backend rt))
                          "void")
            return-body (if (or (= (ir2p/kind body) :block)
                                (#{:if :while :let :loop :assign :throw} (ir2p/kind body)))
                          body-code
                          (sp/shader-return backend body-code))]
        (sp/shader-function-decl backend (:name node) param-strs return-type return-body)))))