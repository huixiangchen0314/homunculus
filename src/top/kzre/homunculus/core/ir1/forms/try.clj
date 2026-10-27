(ns top.kzre.homunculus.core.ir1.forms.try
  "try / catch / throw 的 IR1 构建。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

;; ── throw ─────────────────────────────────
(defmethod ir1/form->node 'throw [form]
  (let [[_ expr] form]
    (n/make-throw (ir1/->ir1 expr) (meta form))))

;; ── catch（不是顶层表单，只在 try 内部出现，作为 helper）──
(defn- make-catch-node
  "class/sym 是属性，body 是多表达式 → wrap-body 成单节点。"
  [clause]
  (let [[_ class sym & body] clause]
    (n/make-catch class sym (n/wrap-body (mapv ir1/->ir1 body)) (meta clause))))

;; ── try ───────────────────────────────────
(defmethod ir1/form->node 'try [form]
  (let [[_ & body-parts] form
        body         (take-while #(not (contains? #{'catch 'finally} (first %))) body-parts)
        after-body   (drop (count body) body-parts)
        catches      (take-while #(= 'catch (first %)) after-body)
        finally-part (drop (count catches) after-body)
        finally-expr (when (and (seq finally-part)
                                (= 'finally (ffirst finally-part)))
                       (rest (first finally-part)))
        ir-body      (n/wrap-body (mapv ir1/->ir1 body))
        ir-catches   (mapv make-catch-node catches)
        ir-finally   (when (seq finally-expr)
                       (n/wrap-body (mapv ir1/->ir1 finally-expr)))]
    (n/make-try ir-body ir-catches ir-finally (meta form))))