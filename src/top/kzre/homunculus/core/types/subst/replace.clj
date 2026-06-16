(ns top.kzre.homunculus.core.types.subst.replace
  "通用表达式变量替换。使用多方法遍历 IR2 节点，将指定变量名替换为给定的节点。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmulti replace-expr
          (fn [node _var-name _replacement] (n/kind node)))

;; 变量匹配则替换
(defmethod replace-expr :variable [node var-name replacement]
  (if (= (n/var-name node) var-name) replacement node))

;; 叶子节点直接返回
(defmethod replace-expr :literal [node _ _] node)

(defmethod replace-expr :call [node var-name replacement]
  (n/make-call (replace-expr (n/call-fn node) var-name replacement)
               (mapv #(replace-expr % var-name replacement) (n/call-args node))
               (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :if [node var-name replacement]
  (n/make-if (replace-expr (n/if-test node) var-name replacement)
             (replace-expr (n/if-then node) var-name replacement)
             (when-let [e (n/if-else node)] (replace-expr e var-name replacement))
             (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :let [node var-name replacement]
  ;; 绑定变量不替换，仅替换值表达式和 body（假设已 alpha 重命名避免阴影）
  (let [new-bindings (mapv (fn [[v e]] [v (replace-expr e var-name replacement)])
                           (n/let-bindings node))
        new-body     (replace-expr (n/let-body node) var-name replacement)]
    (n/make-let new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod replace-expr :block [node var-name replacement]
  (n/make-block (mapv #(replace-expr % var-name replacement) (n/block-exprs node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :loop [node var-name replacement]
  (let [new-bindings (mapv (fn [[v e]] [v (replace-expr e var-name replacement)])
                           (n/loop-bindings node))
        new-body     (replace-expr (n/loop-body node) var-name replacement)]
    (n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod replace-expr :while [node var-name replacement]
  (n/make-while (replace-expr (n/while-test node) var-name replacement)
                (replace-expr (n/while-body node) var-name replacement)
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :assign [node var-name replacement]
  (n/make-assign (replace-expr (n/assign-var node) var-name replacement)
                 (replace-expr (n/assign-val node) var-name replacement)
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :try [node var-name replacement]
  (n/make-try (replace-expr (n/try-body node) var-name replacement)
              (mapv #(replace-expr % var-name replacement) (n/try-catches node))
              (when-let [f (n/try-finally node)] (replace-expr f var-name replacement))
              (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :catch [node var-name replacement]
  (n/make-catch (n/catch-class node) (n/catch-sym node)
                (mapv #(replace-expr % var-name replacement) (n/catch-body node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :throw [node var-name replacement]
  (n/make-throw (replace-expr (n/throw-expr node) var-name replacement)
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :vector [node var-name replacement]
  (n/make-vector (mapv #(replace-expr % var-name replacement) (n/vector-items node))
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :map [node var-name replacement]
  (n/make-map (mapv #(replace-expr % var-name replacement) (n/map-kvs node))
              (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :lambda [node var-name replacement]
  (n/make-lambda (mapv #(replace-expr % var-name replacement) (n/lambda-params node))
                 (replace-expr (n/lambda-body node) var-name replacement)
                 (n/lambda-captures node)
                 (n/lambda-fn-name node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :define [node var-name replacement]
  (n/make-define (n/define-name node)
                 (replace-expr (n/define-val node) var-name replacement)
                 (n/define-doc node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :recur [node var-name replacement]
  (n/make-recur (mapv #(replace-expr % var-name replacement) (n/recur-args node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :convert [node var-name replacement]
  (n/make-convert (replace-expr (n/convert-expr node) var-name replacement)
                  (n/convert-src-ty node) (n/convert-dst-ty node) (n/convert-cost node)
                  (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod replace-expr :member-access [node var-name replacement]
  (n/make-member-access (replace-expr (n/access-target node) var-name replacement)
                        (n/access-member node)
                        (mapv #(replace-expr % var-name replacement) (n/access-args node))
                        (n/node-meta node) (n/parent node)))

;; 无子节点类型
(defmethod replace-expr :ns [node _ _] node)
(defmethod replace-expr :record [node _ _] node)
(defmethod replace-expr :protocol [node _ _] node)

(defmethod replace-expr :default [node _ _] node)

;; 对外接口
(defn replace-var [node var-name replacement]
  (replace-expr node var-name replacement))