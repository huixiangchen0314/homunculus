(ns top.kzre.homunculus.core.irstmt.pass.declare.core
  "var-decl 重建。

   ── 算法 ──

   用 rreduce-children 后向遍历 block。env 跟随遍历——每个
   节点处理时更新 env（use-var / bind-var），不是脱离 env
   单独收集 use。

   assign `x = e`：
   ▸ e 的变量 → 遍历 rhs 时 use-var
   ▸ x unbound → x 的 assign 是声明点 → 转 VarDecl
   ▸ x bound   → 保留 assign

   子 block 递归——env-out - env-in 是子 block 内新增的绑定，
   提升到父 block 声明。"
  (:require
    [clojure.set :as set]
    [top.kzre.homunculus.core.irstmt.ast :as ast]
    [top.kzre.homunculus.core.irstmt.pass.declare.env :as env]))

;; ═══════════════════════════════════════════
;; env-following 的 use 遍历
;; ═══════════════════════════════════════════

(defn- walk-uses
  "遍历节点，每个 variable 调 use-var 更新 env。

   env 跟随遍历——接受 env、返回 env'，不是脱离 env 收集。

   ▸ :variable    —— use-var
   ▸ :if / :while —— 只遍历 test，子块由 decl-stmt 处理
   ▸ :block       —— 子块由 decl-block 处理，不在这里递归
   ▸ :assign      —— lhs 是 def，只遍历 rhs
   ▸ :throw       —— 遍历 expr
   ▸ :default     —— 遍历所有 children"
  [node env-in]
  (if (nil? node)
    env-in
    (case (ast/kind node)
      :variable (env/use-var env-in (:name node))
      :if       (walk-uses (:test node) env-in)
      :while    (walk-uses (:test node) env-in)
      :block    env-in
      :assign   (walk-uses (:val node) env-in)
      :throw    (walk-uses (:expr node) env-in)
      (reduce (fn [env-acc child]
                (walk-uses child env-acc))
              env-in
              (ast/children node)))))

;; ═══════════════════════════════════════════
;; 单 stmt 处理
;; ═══════════════════════════════════════════

(defmulti decl-stmt
          "对单个 stmt 后向收集 use，构造声明。返回 [stmt' env-out]。"
          (fn [stmt _env] (ast/kind stmt)))
;; ═══════════════════════════════════════════
;; block 后向遍历
;; ═══════════════════════════════════════════

(defn- decl-block
  [block env-in]
  (let [env-with-ret (if-let [ret (:ret block)]
                       (walk-uses ret env-in)
                       env-in)
        [block' env-out] (ast/rreduce-children block decl-stmt env-with-ret)]
    [block' env-out]))


(defmethod decl-stmt :assign
  [stmt env-in]
  (let [lhs-var  (:var stmt)
        lhs-name (:name lhs-var)
        env-rhs  (walk-uses (:val stmt) env-in)]
    (if (env/unbound-var? env-rhs lhs-name)
      ;; 声明点——转 VarDecl。attrs 从 lhs 的 Variable 继承
      [(ast/->VarDecl lhs-name
                      (:val stmt)
                      nil
                      (ast/attrs lhs-var)
                      (ast/node-meta stmt))
       (env/bind-var env-rhs lhs-name)]
      ;; 已绑定——保留 assign
      [stmt env-rhs])))

(defmethod decl-stmt :if
  [stmt env-in]
  (let [env-test         (walk-uses (:test stmt) env-in)
        [then' then-env] (decl-block (:then stmt) env-test)
        [else' else-env] (if-let [else (:else stmt)]
                           (decl-block else env-test)
                           [nil env-test])
        escaped          (set/difference
                           (set/union (:bound then-env) (:bound else-env))
                           (:bound env-test))]
    [(ast/->If (:test stmt) then' else'
               (ast/attrs stmt) (ast/node-meta stmt))
     (reduce env/bind-var env-test escaped)]))

(defmethod decl-stmt :while
  [stmt env-in]
  (let [env-test         (walk-uses (:test stmt) env-in)
        [body' body-env] (decl-block (:body stmt) env-test)
        escaped          (set/difference (:bound body-env) (:bound env-test))]
    [(ast/->While (:test stmt) body'
                  (ast/attrs stmt) (ast/node-meta stmt))
     (reduce env/bind-var env-test escaped)]))

(defmethod decl-stmt :block
  [stmt env-in]
  (decl-block stmt env-in))

(defmethod decl-stmt :throw
  [stmt env-in]
  [stmt (walk-uses (:expr stmt) env-in)])

(defmethod decl-stmt :default
  [stmt env-in]
  [stmt (walk-uses stmt env-in)])



;; ═══════════════════════════════════════════
;; function / method
;; ═══════════════════════════════════════════

(defn- decl-function
  [fn-node]
  (let [params      (set (map :name (:params fn-node)))
        env-initial (reduce env/bind-var (env/make-env) params)
        [body' _]   (decl-block (:body fn-node) env-initial)]
    (assoc fn-node :body body')))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn decl
  "对 IRStmt 节点列表重建 var-decl。"
  [nodes]
  (mapv (fn [node]
          (case (ast/kind node)
            (:function :method) (decl-function node)
            node))
        nodes))