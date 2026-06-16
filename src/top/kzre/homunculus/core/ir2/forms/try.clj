(ns top.kzre.homunculus.core.ir2.forms.try
  "try / catch / throw 的 IR2 lowering。"
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

;; ── try ───────────────────────────────────
(defmethod ir2/lower-ast :try [node env]
  (let [body-ir1    (n1/try-body node)            ;; 单个 IR1 节点 (已包装)
        catches-ir1 (n1/try-catches node)         ;; IR1 CatchNode 列表
        finally-ir1 (n1/try-finally node)         ;; 单个 IR1 节点或 nil
        ;; lowering 主体
        ir-body     (first (ir2/lower-ast body-ir1 env))
        ;; lowering 每个 catch
        ir-catches  (mapv (fn [c]
                            (let [class-ir1 (n1/catch-class c)
                                  sym-ir1   (n1/catch-sym c)
                                  body-ir1s (n1/catch-body c)   ;; 向量
                                  class-node (first (ir2/lower-ast class-ir1 env))
                                  sym-node   (first (ir2/lower-ast sym-ir1 env))
                                  body-nodes (mapv #(first (ir2/lower-ast % env)) body-ir1s)]
                              (n2/make-catch class-node sym-node body-nodes
                                             {} (n1/node-meta c) nil)))
                          catches-ir1)
        ;; lowering finally（若存在）
        ir-finally  (when finally-ir1
                      (first (ir2/lower-ast finally-ir1 env)))]
    [(n2/make-try ir-body ir-catches ir-finally {} (n1/node-meta node) nil)]))

;; ── throw ─────────────────────────────────
(defmethod ir2/lower-ast :throw [node env]
  (let [expr (first (ir2/lower-ast (n1/throw-expr node) env))]
    [(n2/make-throw expr {} (n1/node-meta node) nil)]))