(ns top.kzre.homunculus.core.irstmt.pass.declare.polyfill
  "var-decl polyfill——CFG 优化之后重建声明。

   ── 策略：块级 LCA 声明 ──

   变量声明在它所有出现（def/use）的最近公共祖先块（LCA）
   的开头。无初值 VarDecl。所有 assign 保持 Assign。

   ▸ 只在一个 while body 内用的变量——声明在 while body 内
   ▸ 在 then 和 else 都 def、if 之后 use——声明在 if 所在的
     父 block 开头
   ▸ 在函数和子块都 use——声明在函数开头

   ── 后向遍历 ──

   env = {var-name → attrs}，记录“该位置之后被 use/def 的
   变量”。后向遍历（rreduce-children 从右到左）累积 env。

   退出 block 时：need-decl = env-out - env-in（key 差集）——
   只在 block 内使用/定义的变量——LCA 是当前 block——在当前
   block 开头插入无初值 VarDecl。

   ── 一个多方法 ──

   polyfill-node 处理所有节点——expr 和 stmt 统一。返回
   [node' env']。子节点递归自动分发到对应 defmethod。"
  (:require
    [clojure.set :as set]
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

;; ═══════════════════════════════════════════
;; 统一多方法
;; ═══════════════════════════════════════════

(defmulti polyfill-node
          "处理节点，返回 [node' env']。

           env = {var-name → attrs}——记录“之后被 use/def 的变量”。"
          (fn [node _env] (ast/kind node)))

(defmethod polyfill-node :variable
  [node env]
  (if (contains? env (:name node))
    [node env]
    [node (assoc env (:name node) (ast/attrs node))]))

(defmethod polyfill-node :assign
  [stmt env]
  (let [lhs-var        (:var stmt)
        lhs-name       (:name lhs-var)
        [val' env-rhs] (polyfill-node (:val stmt) env)
        env-out        (if (contains? env-rhs lhs-name)
                         env-rhs
                         (assoc env-rhs lhs-name (ast/attrs lhs-var)))]
    [(assoc stmt :val val') env-out]))

(defmethod polyfill-node :if
  [stmt env]
  (let [[test' env-test] (polyfill-node (:test stmt) env)
        [then' then-env] (polyfill-node (:then stmt) env-test)
        [else' else-env] (if-let [else (:else stmt)]
                           (polyfill-node else env-test)
                           [nil env-test])
        env-out          (merge then-env else-env)]
    [(ast/->If test' then' else'
               (ast/attrs stmt) (ast/node-meta stmt))
     env-out]))

(defmethod polyfill-node :while
  [stmt env]
  (let [[test' env-test] (polyfill-node (:test stmt) env)
        [body' body-env] (polyfill-node (:body stmt) env-test)]
    [(ast/->While test' body'
                  (ast/attrs stmt) (ast/node-meta stmt))
     body-env]))

(defmethod polyfill-node :block
  [block env-in]
  (let [[ret' env-with-ret] (if-let [ret (:ret block)]
                              (polyfill-node ret env-in)
                              [nil env-in])
        [stmts' env-out]    (ast/rreduce-children
                              block
                              (fn [stmt env]
                                (polyfill-node stmt env))
                              env-with-ret)
        need-decl-names     (set/difference (set (keys env-out))
                                            (set (keys env-in)))
        decl-stmts          (mapv (fn [var-name]
                                    (ast/->VarDecl var-name
                                                   nil nil
                                                   (get env-out var-name)
                                                   nil))
                                  (sort-by str need-decl-names))
        block'              (-> block
                                (assoc :stmts (into decl-stmts stmts'))
                                (assoc :ret ret'))]
    [block' env-out]))

(defmethod polyfill-node :throw
  [stmt env]
  (let [[expr' env'] (polyfill-node (:expr stmt) env)]
    [(assoc stmt :expr expr') env']))

(defmethod polyfill-node :default
  [node env]
  (let [[node' env'] (ast/rreduce-children
                       node
                       (fn [child env-acc]
                         (polyfill-node child env-acc))
                       env)]
    [node' env']))

;; ═══════════════════════════════════════════
;; function / method
;; ═══════════════════════════════════════════

(defn polyfill-function
  [fn-node]
  (let [params      (:params fn-node)
        env-initial (into {}
                          (map (fn [p] [(:name p) (ast/attrs p)]))
                          params)
        [body' _]   (polyfill-node (:body fn-node) env-initial)]
    (assoc fn-node :body body')))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn polyfill
  "对 IRStmt 节点列表重建 var-decl。

   块级 LCA 声明——变量声明在它所有出现的最近公共祖先块开头。"
  [nodes]
  (mapv (fn [node]
          (case (ast/kind node)
            (:function :method) (polyfill-function node)
            node))
        nodes))