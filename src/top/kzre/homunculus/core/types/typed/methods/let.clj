(ns top.kzre.homunculus.core.types.typed.methods.let
  (:require [clojure.set :as set]
            [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.types.typed.scheme :as s]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import (top.kzre.homunculus.core.types.typed.scheme TScheme)))

(defmethod infer/infer :let [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node {}]
    (let [bindings (:bindings node)
          [bind-nodes env-ext s-bindings]
          (reduce (fn [[bnds env subst] [var-node val-node]]
                    (let [[val-ty val-node' s-val] (infer/infer val-node (assoc context :env env))
                          ;; 合并当前替换和该绑定的替换
                          subst' (merge subst s-val)
                          ;; 应用合并后的替换到值类型和标注类型
                          val-ty' (u/substitute val-ty subst')
                          annot-ty (when-let [f (:frontend context)] (tp/meta->type f var-node))
                          env-subst (infer/apply-subst-to-env env subst')
                          ;; 决定绑定类型：有标注时，统一标注与值类型，使用标注类型（单态）
                          binding (if annot-ty
                                    (let [annot-ty' (u/substitute annot-ty subst')]
                                      (u/unify val-ty' annot-ty')   ;; 确保值类型与标注兼容
                                      annot-ty')                     ;; 存储标注类型（单态）
                                    (s/generalize val-ty' env-subst))
                          var-name (:name var-node)
                          env2 (e/extend-env env var-name binding)
                          var-node' (if (instance? TScheme binding)
                                      var-node
                                      (assoc-in var-node [:attrs :type] binding))]
                      [(conj bnds [var-node' val-node']) env2 subst']))
                  [[] (:env context) {}]
                  bindings)
          env-body (infer/apply-subst-to-env env-ext s-bindings)
          [body-ty body-node s-body] (infer/infer (:body node) (assoc context :env env-body))
          s-final (merge s-bindings s-body)
          overall-ty (u/substitute body-ty s-final)
          new-attrs (assoc (ir2p/attrs node) :type overall-ty)
          new-node (assoc node :bindings (vec bind-nodes) :body body-node :attrs new-attrs)]
      [overall-ty new-node s-final])))