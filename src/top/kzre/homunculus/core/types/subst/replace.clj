(ns top.kzre.homunculus.core.types.subst.replace
  "通用表达式变量替换。利用 reduce-children 协议遍历 IR2 节点。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.ast :as p]))

(defmulti replace-expr
          (fn [node _var-name _replacement] (n/kind node)))

;; ── 变量匹配：直接替换 ──────────────────────
(defmethod replace-expr :variable [node var-name replacement]
  (if (= (n/var-name node) var-name) replacement node))

;; ── 需要保留绑定侧变量的特殊形式 ────────────
;; 这些节点的绑定变量符号不参与替换（假设已 alpha 重命名）
(defmethod replace-expr :let [node var-name replacement]
  (let [new-bindings (mapv (fn [b]
                             (assoc b :val (replace-expr (:val b) var-name replacement)))
                           (n/let-bindings node))
        new-body     (replace-expr (n/let-body node) var-name replacement)]
    (n/make-let new-bindings new-body (n/attrs node) (n/node-meta node))))

(defmethod replace-expr :loop [node var-name replacement]
  (let [new-bindings (mapv (fn [b]
                             (assoc b :val (replace-expr (:val b) var-name replacement)))
                           (n/loop-bindings node))
        new-body     (replace-expr (n/loop-body node) var-name replacement)]
    (n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node))))

(defmethod replace-expr :lambda [node var-name replacement]
  ;; 参数不替换，只替换 body
  (n/make-lambda (n/lambda-params node)
                 (replace-expr (n/lambda-body node) var-name replacement)
                 (n/lambda-captures node)
                 (n/lambda-fn-name node)
                 (n/attrs node) (n/node-meta node)))

(defmethod replace-expr :catch [node var-name replacement]
  ;; catch 的异常符号不替换，只替换 body
  (n/make-catch (n/catch-class node) (n/catch-sym node)
                (mapv #(replace-expr % var-name replacement) (n/catch-body node))
                (n/attrs node) (n/node-meta node)))

;; ── 默认：利用 reduce-children 自动遍历所有子节点 ──
(defmethod replace-expr :default [node var-name replacement]
  (let [[new-node _] (p/reduce-children node
                                        (fn [child _] [(replace-expr child var-name replacement) nil])
                                        nil)]
    new-node))

;; ── 对外接口 ─────────────────────────────────
(defn replace-var [node var-name replacement]
  (replace-expr node var-name replacement))