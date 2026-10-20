(ns top.kzre.homunculus.core.irstmt.pass.role-fold.transform
  "角色变换：给定 function / method 节点和 RoleTable，返回变换后的节点。

   不是 pass——不做迭代，不递归 function / method。
   一次调用执行一轮变换，多轮迭代由 role/core 负责。

   ══════════════════════════════════════════════════════════════
   变换计划（Plan）
   ══════════════════════════════════════════════════════════════

   从 RoleTable 构建三类操作：

   1. subst-map:  {old-name → new-name}
      所有 variable 节点的名字按此替换。

   2. rename-map: {old-decl-name → new-name}
      所有 var-decl 的 name 按此替换。与 subst-map 分离的原因是
      var-decl 的 name 是符号而非 variable 节点。

   3. delete-nodes: [node ...]
      这些节点从 IR 里删除（用 identical? 匹配节点实例）。

   ══════════════════════════════════════════════════════════════
   变换规则（数组链）
   ══════════════════════════════════════════════════════════════

   仅处理完整链（accumulator → collector → receiver）。
   receiver 的声明位置合并到 accumulator 声明的位置：

     - accumulator 的 var-decl：改名到 receiver 名（保留原位置，无 init）
     - receiver 的 var-decl：删除
     - collector 的 var-decl：删除
     - collector 的所有 aset：删除
     - accumulator / collector 的变量引用：按 subst-map 替换为 receiver 名

   部分链（accumulator 有 collector 但无 receiver，或 accumulator 有
   receiver 但无 collector）保守跳过，交给 DCE 处理。

   ══════════════════════════════════════════════════════════════
   变换规则（if 链）
   ══════════════════════════════════════════════════════════════

   仅处理有 if-receiver 的 if-collector：

     - if-collector 的 var-decl：改名到 receiver 名
     - if-receiver 的 var-decl：删除
     - if-collector 的 then/else assign：
         · val 是另一个已识别 if-collector → 删除（该 assign 属于传递链）
         · 否则 → 保留，通过 subst-map 改目标为 receiver 名

   多轮迭代：一轮变换后，剩下的内层 if-collector 可能在下一轮被
   analyze 识别（如果内层 if 的两个分支在变换后暴露出来）。这依赖
   role/core 的外层不动点。

   前提：如果 analyze 已识别所有层的 if-collector（含嵌套），
   transform 一次就能处理完所有层。如果只识别了部分，transform
   保底处理已识别的部分，语义仍然正确（未识别的部分保持原样）。"
  (:require
    [top.kzre.homunculus.core.irstmt.ast :as ast]
    [top.kzre.homunculus.core.irstmt.pass.role-fold.role :as role]))

(defn- variable-node? [n]
  (and n (= :variable (ast/kind n))))

;; ═══════════════════════════════════════════
;; Plan 构建
;; ═══════════════════════════════════════════

(defn- build-array-plan
  "为一条完整的数组链（accumulator → collector → receiver）构建变换操作。"
  [table receiver]
  (let [recv-name (:var-name receiver)
        col-name  (:source receiver)
        col       (role/collector table col-name)]
    (when col
      (let [arr-name (:source col)
            arr      (role/accumulator table arr-name)]
        (when arr
          {:subst      {arr-name recv-name
                        col-name recv-name}
           :rename     {arr-name recv-name}
           :delete-set (set (concat [(:var-decl-node receiver)
                                     (:var-decl-node col)]
                                    (:aset-nodes col)))})))))

(defn- build-if-plan
  "为一条 if 链（if-collector → if-receiver）构建变换操作。

   then/else assign 中 val 是另一个 if-collector 名 → 删除该 assign
   （该内层 if-collector 自己的 then/else assign 会通过 subst-map 重定向）。
   否则 → 通过 subst-map 改目标为 receiver 名，保留 assign。"
  [table receiver]
  (let [recv-name (:var-name receiver)
        ifc-name  (:source receiver)
        ifc       (role/if-collector table ifc-name)]
    (when ifc
      (let [then-assign (:then-assign ifc)
            else-assign (:else-assign ifc)
            passes-through?
            (fn [a]
              (let [v (:val a)]
                (and (variable-node? v)
                     (role/if-collector? table (:name v)))))
            delete-assigns (filter passes-through?
                                   [then-assign else-assign])]
        {:subst      {ifc-name recv-name}
         :rename     {ifc-name recv-name}
         :delete-set (set (cons (:var-decl-node receiver)
                                delete-assigns))}))))

(defn- build-plan
  "从 RoleTable 构建完整变换计划。"
  [table]
  (let [array-plans (keep #(build-array-plan table %) (vals (:receivers table)))
        if-plans    (keep #(build-if-plan table %)    (vals (:if-receivers table)))
        all-plans   (concat array-plans if-plans)]
    {:subst-map    (reduce merge {} (map :subst all-plans))
     :rename-map   (reduce merge {} (map :rename all-plans))
     :delete-nodes (vec (mapcat :delete-set all-plans))}))

;; ═══════════════════════════════════════════
;; Plan 应用
;; ═══════════════════════════════════════════

(defn- apply-plan
  [node {:keys [subst-map rename-map delete-nodes]}]
  (let [delete? (fn [n] (some #(identical? % n) delete-nodes))
        rename-var (fn [old] (get subst-map old old))
        rename-decl (fn [old] (get rename-map old old))]
    (letfn [(walk [n]
              (cond
                (nil? n) nil
                (delete? n) nil

                (= :variable (ast/kind n))
                (let [new-name (rename-var (:name n))]
                  (if (= new-name (:name n))
                    n
                    (assoc n :name new-name)))

                (= :var-decl (ast/kind n))
                (let [new-name (rename-decl (:name n))
                      init'    (walk (:val n))]
                  (assoc n :name new-name :val init'))

                (= :block (ast/kind n))
                (let [stmts' (vec (keep walk (:stmts n)))
                      ret'   (walk (:ret n))]
                  (ast/->Block stmts' ret'
                               (ast/attrs n) (ast/node-meta n)))

                :else
                (let [[new-n _]
                      (ast/reduce-children
                        n
                        (fn [child env] [(walk child) env])
                        nil)]
                  new-n)))]
      (walk node))))

;; ═══════════════════════════════════════════
;; 主入口
;; ═══════════════════════════════════════════

(defn transform
  "给定 function / method 节点和 RoleTable，返回变换后的节点。

   一次调用执行一轮变换，不做不动点迭代。
   多轮迭代由 role/core 负责。

   变换前提：
   - RoleTable 是 analyze 在同一节点上产出的（识别的是同一份 IR）
   - 节点实例引用有效（analyze 和 transform 之间没有别的 pass 修改过 IR）

   返回值：
   - 变换后的节点（可能是新对象，也可能原样返回）
   - 若 RoleTable 中没有完整链可变换，返回原节点"
  [node table]
  (let [plan (build-plan table)]
    (if (or (empty? (:subst-map plan))
            (empty? (:delete-nodes plan)))
      node
      (apply-plan node plan))))