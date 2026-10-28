(ns top.kzre.homunculus.frontend.shader.pass.loopfor.core
  "while 循环转 for 循环。

   ── 两种输出 ──

   ▸ 自然循环——检查全过——转完整 for：

       for (int i = 0; i < n; i = i + 1) { loop-stmts }
       exit-stmts;

     init 合并到 for 的 init；如果 exit-stmts use 归纳变量，则 init
     保留在 for 外（`int i = 0; for (; cond; update) { ... }`），因为
     for 里声明的 i 在 for 外不可见。

     多归纳变量时——第一个作为 for 的 update，其余留在 body 末尾。
     other 归纳变量的 init 保留在 stmts 里（不合并）。

   ▸ 退化形式——其他情况——转 for(;test;)：

       for (; test;) { body }

     init 保持为 for 之前的独立 stmt，update 保持为 body 末尾的 stmt。

   ── 相关符号的处理 ──

   自然循环转换后：
   ▸ while 之前的归纳变量 var-decl —— 合并到 for 的 init，或
     exit-stmts use 它时保留在 for 外
   ▸ while 之前的 flag var-decl —— 删除（for 的 test 取代）；
     找不到则保留
   ▸ other 归纳变量的 var-decl —— 保留在 stmts 里

   ── 配置 ──

   :shader-all-loopfor? —— true（默认）时退化形式也转 for 关键字，
   false 时只转自然循环，其余保持 while。"
  (:require
    [top.kzre.homunculus.frontend.shader.ast :as ast]
    [top.kzre.homunculus.frontend.shader.pass.loopfor.rotate :as rotate]
    [top.kzre.homunculus.frontend.shader.pass.loopfor.env :as env]))

;; ═══════════════════════════════════════════
;; use 检查
;; ═══════════════════════════════════════════

(defn- any-use?
  [stmts var]
  (boolean (some #(contains? (rotate/stmt-uses %) var) stmts)))

;; ═══════════════════════════════════════════
;; decl 查找
;; ═══════════════════════════════════════════

(defn- find-decl-idx
  "从 stmts 的 idx 位置往前扫描，找 name 的 var-decl 索引。
   遇到非 var-decl 的 stmt 停止——保守——只考虑紧邻的 decl 链。"
  [stmts idx name]
  (loop [i (dec idx)]
    (when (>= i 0)
      (let [stmt (nth stmts i)]
        (if (= :var-decl (ast/kind stmt))
          (if (= name (:name stmt))
            i
            (recur (dec i)))
          nil)))))

;; ═══════════════════════════════════════════
;; 变换
;; ═══════════════════════════════════════════

(defn- negate
  [expr]
  (ast/->UnaryOp '! expr (ast/attrs expr) (ast/node-meta expr)))

(defn- make-degraded-for
  [while-node]
  (ast/->For nil (:test while-node) nil (:body while-node)
             (ast/attrs while-node) (ast/node-meta while-node)))

(defn- plan-natural
  "对自然循环构造 for 节点和前置 decl 的删除集合。

   返回 {:for for-node :exit-stmts [...] :remove-idx #{...}}。"
  [stmts idx analysis]
  (let [{:keys [flag guard exit-stmts loop-stmts update
                induction-var branch-kind]} analysis
        init-idx      (find-decl-idx stmts idx induction-var)
        flag-idx      (find-decl-idx stmts idx flag)
        init-decl     (when init-idx (nth stmts init-idx))
        test-expr     (if (= :exit-then branch-kind)
                        (negate guard) guard)
        exit-uses-iv? (any-use? exit-stmts induction-var)
        for-init      (when (and init-decl (not exit-uses-iv?)) init-decl)
        while-node    (nth stmts idx)
        for-body      (ast/->Block (vec loop-stmts) nil
                                   (ast/attrs while-node)
                                   (ast/node-meta while-node))
        for-node      (ast/->For for-init test-expr update for-body
                                 (ast/attrs while-node)
                                 (ast/node-meta while-node))
        removed       (cond-> #{}
                              flag-idx (conj flag-idx)
                              (and init-idx (not exit-uses-iv?)) (conj init-idx))]
    {:for for-node
     :exit-stmts (vec exit-stmts)
     :remove-idx removed}))

(defn- transform-block
  "处理 block 的 stmts。返回新的 stmts 列表。

   两阶段：
   ▸ 扫描——对每个 while 调 rotate/analyze-while，构造 for，标记
     要删的前置 decl
   ▸ 重建——按扫描结果重新组装 stmts

   env 贯穿——all-loopfor? 只在这里查一次，用于 degraded 分支。"
  [stmts env]
  (let [all? (env/all-loopfor? env)
        {:keys [new-nodes remove-idx]}
        (reduce
          (fn [{:keys [new-nodes remove-idx]} [i stmt]]
            (if (not= :while (ast/kind stmt))
              {:new-nodes new-nodes :remove-idx remove-idx}
              (let [analysis (rotate/analyze-while stmt)]
                (case (:kind analysis)
                  :natural
                  (let [{:keys [for exit-stmts] plan-remove :remove-idx}
                        (plan-natural stmts i analysis)]
                    {:new-nodes  (assoc new-nodes i (into [for] exit-stmts))
                     :remove-idx (into remove-idx plan-remove)})

                  :degraded
                  (if all?
                    {:new-nodes  (assoc new-nodes i [(make-degraded-for stmt)])
                     :remove-idx remove-idx}
                    {:new-nodes new-nodes :remove-idx remove-idx})

                  :not-while
                  {:new-nodes new-nodes :remove-idx remove-idx}))))
          {:new-nodes {} :remove-idx #{}}
          (map-indexed vector stmts))]
    (->> stmts
         (keep-indexed
           (fn [i stmt]
             (cond
               (contains? new-nodes i) (get new-nodes i)
               (contains? remove-idx i) nil
               :else [stmt])))
         (apply concat)
         vec)))

;; ═══════════════════════════════════════════
;; 遍历 AST——env 全程传递
;; ═══════════════════════════════════════════

(defn- transform-node
  [node env]
  (case (ast/kind node)
    :block
    (let [stmts'  (mapv #(transform-node % env) (:stmts node))
          stmts'' (transform-block stmts' env)]
      (assoc node
        :stmts stmts''
        :ret (when-let [r (:ret node)]
               (transform-node r env))))

    :if
    (-> node
        (update :then #(transform-node % env))
        (update :else #(when % (transform-node % env))))

    :while
    (update node :body #(transform-node % env))

    :for
    (-> node
        (update :init #(when % (transform-node % env)))
        (update :update #(when % (transform-node % env)))
        (update :body #(transform-node % env)))

    :function
    (update node :body #(transform-node % env))

    :entry-point
    (update node :body #(transform-node % env))

    node))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn run
  "对 ShaderAST 的顶层节点列表做 while→for 转换。"
  [asts env]
  (mapv #(transform-node % env) asts))