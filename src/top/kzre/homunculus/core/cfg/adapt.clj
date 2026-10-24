(ns top.kzre.homunculus.core.cfg.adapt
  "CFG 的前端适配协议。

   ── 节点构造的约定 ──

   template 是定义该变量的原始语句节点（assign / var-decl）。

   分析结果前向传播——IR2 分析时候语句都是表达式，分析结果
   （类型等 attrs）会一路向上传递到语句节点。所以直接复制
   template 的 :attrs / :meta 即可，不需要提取内部的 variable
   节点——template 上的 attrs 描述的就是该语句产生的值的类型，
   也就是被定义变量的类型。

   template 必须非 nil——nil 抛异常，不能静默构造空 attrs 节点。

   ── aset 和 call 一样 ──

   aset(arr, i, v) 和有副作用的 call 一样——不定义简单变量，
   uses 是 {arr, i, v}，defs 是 ∅。数组不做 SSA，不产生新版本。
   这和 call 完全一致：有副作用但不改变变量身份。"
  )
(defprotocol IAdapter
  ;; ── 表达式级 ──
  (expr-uses [this expr])
  (expr-replace-uses [this expr subst])

  ;; ── 语句级 ──
  (stmt-uses [this stmt])
  (stmt-defs [this stmt])
  (stmt-replace-uses [this stmt subst])
  (stmt-replace-defs [this stmt subst])

  ;; ── 终结指令级 ──
  (term-uses [this term])
  (term-replace-uses [this term subst])

  ;; ── 节点构造 ──
  (make-var-ref [this origin sym]
    "构造变量引用节点。origin 非 nil——直接复制 origin 的
     :attrs / :meta，只替换 :name 为 sym。

     分析结果前向传播——语句节点的 attrs 描述其产生值的类型，
     即被定义变量的类型。不需要提取内部 variable 节点。")

  (make-move [this origin var-sym value]
    "构造 move 语句：var-sym = expr。

     origin 非 nil——var 节点和 assign 节点都从 origin 复制
     :attrs / :meta。")

  ;; ── 节点构造（bake 用）──
  (make-block [this stmts ret]
    "构造 block 节点。origin 非 nil 时从 origin 继承 :attrs / :meta。
     origin 为 nil 时用空 attrs / meta。")

  (make-if [this test then-block else-block]
    "构造 if 节点。origin 非 nil 时从 origin 继承 :attrs / :meta。")

  (make-throw [this expr]
    "构造 throw 语句。origin 非 nil 时从 origin 继承 :attrs / :meta。")
  (make-while [this test body-block]
    "构造 while 节点。IRStmt 定义：[:expr 'test :block 'body 'attrs]。
     origin 非 nil 时从 origin 继承 :attrs / :meta。"))

;; ═══════════════════════════════════════════
;; 通用辅助
;; ═══════════════════════════════════════════

(defn block-uses-defs
  "汇总一个基本块的所有 uses / defs。

   uses —— 块内被读、且读之前未在块内定义的变量。
   defs —— 块内被定义的所有变量。

   终结指令的 uses 也计入。"
  [adapter block]
  (let [stmts    (:stmts block)
        term     (:term block)
        {:keys [uses defs]}
        (reduce (fn [{:keys [uses defs] :as acc} stmt]
                  (let [s-uses (stmt-uses adapter stmt)
                        s-defs (stmt-defs adapter stmt)
                        new-uses (reduce (fn [s u]
                                           (if (contains? defs u) s (conj s u)))
                                         uses s-uses)]
                    {:uses new-uses
                     :defs (into defs s-defs)}))
                {:uses #{} :defs #{}}
                stmts)
        term-u (term-uses adapter term)]
    {:uses (reduce (fn [s u]
                     (if (contains? defs u) s (conj s u)))
                   uses term-u)
     :defs defs}))