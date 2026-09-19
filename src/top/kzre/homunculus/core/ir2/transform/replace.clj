(ns top.kzre.homunculus.core.ir2.transform.replace
  "通用表达式变量替换。

   语义保持变换：把子树中对 var-name 的引用替换为 replacement。
   不处理绑定遮蔽——调用方保证 var-name 不在子树内被重新绑定
   （通常由 alpha 重命名保证）。"
  (:require
    [top.kzre.homunculus.core.ir2.ast :as p]
    [top.kzre.homunculus.core.ir2.node :as n]))

;; ═══════════════════════════════════════════════
;; 多方法分派
;; ═══════════════════════════════════════════════

(defmulti replace*
          (fn [node _var-name _replacement] (n/kind node)))

;; ── 变量匹配：直接替换 ──
(defmethod replace* :variable
  [node var-name replacement]
  (if (= (n/var-name node) var-name) replacement node))

;; ── 顺序作用域：绑定变量不替换 ──
(defmethod replace* :let
  [node var-name replacement]
  (let [new-bindings (mapv (fn [b]
                             (assoc b :val (replace* (:val b) var-name replacement)))
                           (n/let-bindings node))
        new-body     (replace* (n/let-body node) var-name replacement)]
    (n/make-let new-bindings new-body
                (n/attrs node) (n/node-meta node))))

(defmethod replace* :loop
  [node var-name replacement]
  (let [new-bindings (mapv (fn [b]
                             (assoc b :val (replace* (:val b) var-name replacement)))
                           (n/loop-bindings node))
        new-body     (replace* (n/loop-body node) var-name replacement)]
    (n/make-loop new-bindings new-body
                 (n/attrs node) (n/node-meta node))))

;; ── lambda：params 不替换，只替换 body ──
(defmethod replace* :lambda
  [node var-name replacement]
  (n/make-lambda (n/lambda-params node)
                 (replace* (n/lambda-body node) var-name replacement)
                 (n/lambda-captures node)
                 (n/lambda-fn-name node)
                 (n/attrs node) (n/node-meta node)))

;; ── catch：异常符号不替换，只替换 body ──
(defmethod replace* :catch
  [node var-name replacement]
  (n/make-catch (n/catch-class node)
                (n/catch-sym node)
                (mapv #(replace* % var-name replacement) (n/catch-body node))
                (n/attrs node) (n/node-meta node)))

;; ── 默认：reduce-children 递归 ──
(defmethod replace* :default
  [node var-name replacement]
  (first
    (p/reduce-children
      node
      (fn [child _] [(replace* child var-name replacement) nil])
      nil)))

;; ═══════════════════════════════════════════════
;; 对外
;; ═══════════════════════════════════════════════

(defn replace-var
  [node var-name replacement]
  (replace* node var-name replacement))