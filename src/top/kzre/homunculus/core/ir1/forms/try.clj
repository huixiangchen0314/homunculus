(ns top.kzre.homunculus.core.ir1.forms.try
  "try / catch / throw 的 IR1 构建。所有字段访问通过 ir1.node 工具函数。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

;; ── throw ─────────────────────────────────
(defmethod ir1/form->node 'throw [form]
  (let [[_ expr] form]
    (n/make-throw expr (meta form))))

(defmethod ir1/build-tree :throw [node]
  (n/make-throw (ir1/->ir1 (n/throw-expr node))
                (n/node-meta node)
                (n/parent node)))

;; ── try ───────────────────────────────────
(defmethod ir1/form->node 'try [form]
  (let [[_ & body-parts] form
        body (take-while #(not (contains? #{'catch 'finally} (first %))) body-parts)
        after-body (drop (count body) body-parts)
        catches (take-while #(= 'catch (first %)) after-body)
        finally-part (drop (count catches) after-body)
        finally-expr (when (= 'finally (ffirst finally-part))
                       (rest (first finally-part)))
        ;; 将每个 catch 子句转为原始 CatchNode
        catch-nodes (mapv (fn [clause]
                            (let [[_ class sym & body] clause]
                              (n/make-catch class sym (vec body) nil))) ; meta 暂缺
                          catches)]
    (n/make-try (vec body) catch-nodes finally-expr (meta form))))

(defmethod ir1/build-tree :try [node]
  (let [body-exprs   (n/try-body node)          ;; 原始表单向量（来自 form->node）
        catch-nodes  (n/try-catches node)       ;; CatchNode 列表
        finally-expr (n/try-finally node)       ;; 原始 finally 表单（单个节点？其实是表达式序列）
        meta         (n/node-meta node)
        parent       (n/parent node)
        ;; 构建 body 子节点（向量 -> 合并为单个节点）
        ir-body      (n/wrap-body (mapv ir1/->ir1 body-exprs))
        ;; 构建每个 catch（通过 ir1/->ir1 触发 build-tree :catch）
        ir-catches   (mapv ir1/->ir1 catch-nodes)
        ;; 构建 finally：如果有，将其表达式列表包装为单个节点，否则 nil
        ir-finally   (when finally-expr
                       (n/wrap-body (mapv ir1/->ir1 finally-expr)))]
    (n/make-try ir-body ir-catches ir-finally meta parent)))

;; ── catch ─────────────────────────────────
(defmethod ir1/build-tree :catch [node]
  (n/make-catch (ir1/->ir1 (n/catch-class node))
                (ir1/->ir1 (n/catch-sym node))
                (mapv ir1/->ir1 (n/catch-body node))   ;; body 保持为向量
                (n/node-meta node)
                (n/parent node)))