(ns top.kzre.homunculus.core.ir1.forms.try
  "try / catch / throw 的 IR1 构建。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

;; ── throw ─────────────────────────────────
(defmethod ir1/lower-node* 'throw [form env]
  (let [[_ expr] form
        [expr-node env'] (ir1/lower-node expr env)]
    [(n/make-throw expr-node (meta form)) env']))

;; ── catch（不是顶层表单，只在 try 内部出现，作为 helper）──
(defn- make-catch-node
  "class/sym 是属性，body 是多表达式 → wrap-body 成单节点。
   catch 体是独立作用域，env' 不外传。"
  [clause env]
  (let [[_ class sym & body] clause
        [body-nodes _] (ir1/lower-seq body env)]
    (n/make-catch class sym (n/wrap-body body-nodes) (meta clause))))

;; ── try ───────────────────────────────────
(defmethod ir1/lower-node* 'try [form env]
  (let [[_ & body-parts] form
        body         (take-while #(not (contains? #{'catch 'finally} (first %))) body-parts)
        after-body   (drop (count body) body-parts)
        catches      (take-while #(= 'catch (first %)) after-body)
        finally-part (drop (count catches) after-body)
        finally-expr (when (and (seq finally-part)
                                (= 'finally (ffirst finally-part)))
                       (rest (first finally-part)))
        [body-nodes _]     (ir1/lower-seq body env)
        ir-body            (n/wrap-body body-nodes)
        ir-catches         (mapv #(make-catch-node % env) catches)
        [finally-nodes _]  (when (seq finally-expr)
                             (ir1/lower-seq finally-expr env))
        ir-finally         (when (seq finally-expr)
                             (n/wrap-body finally-nodes))]
    [(n/make-try ir-body ir-catches ir-finally (meta form)) env]))