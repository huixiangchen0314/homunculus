(ns top.kzre.homunculus.core.cfg.irstmt.build
  (:require
   [top.kzre.homunculus.core.cfg.build :as build]
   [top.kzre.homunculus.core.irstmt.ast :as ast]))

;; ═══════════════════════════════════════════
;; IRStmt 不变量校验
;; ═══════════════════════════════════════════

(def ^:private control-flow-kinds
  "控制流节点——不能出现在 block 的 ret 位置。"
  #{:if :while :block :throw :try :catch})

(defn- validate-ret!
  "校验 block 的 ret 是简单表达式。违反 IRStmt 不变量时抛异常。"
  [ret]
  (when (and ret (contains? control-flow-kinds (ast/kind ret)))
    (throw (ex-info
             "IRStmt invariant violated: block ret must be a simple expression"
             {:ret  ret
              :kind (ast/kind ret)
              :hint "statementize 应该把控制流展开到 stmts 里，ret 只承载块的值"})))
  ret)

;; ═══════════════════════════════════════════
;; 规范形式转换
;; ═══════════════════════════════════════════

(defn- block-form
  "IRStmt 的 :block 节点 → 规范 block 形式。"
  [node]
  (cond
    (nil? node)
    {:kind :block :stmts [] :ret nil}

    (= :block (ast/kind node))
    {:kind :block
     :stmts (vec (:stmts node))
     :ret   (validate-ret! (:ret node))}

    :else
    {:kind :block :stmts [] :ret (validate-ret! node)}))

;; ═══════════════════════════════════════════
;; ICFGBuilder 实现
;; ═══════════════════════════════════════════

(defrecord IRStmtCFGBuilder []
  build/ICFGBuilder
  (decon [_ node]
    (case (ast/kind node)
      :if    {:kind :if
              :test (:test node)
              :then (block-form (:then node))
              :else (when (:else node) (block-form (:else node)))}
      :while {:kind :while
              :test (:test node)
              :body (block-form (:body node))}
      :throw {:kind :throw
              :expr (:expr node)}
      :block (block-form node)
      {:kind :stmt})))

(defn builder []
  (->IRStmtCFGBuilder))