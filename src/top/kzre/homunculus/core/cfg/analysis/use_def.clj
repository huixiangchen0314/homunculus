(ns top.kzre.homunculus.core.cfg.analysis.use-def
  "变量访问协议——CFG 上数据流分析的统一接口。

   ── 为什么需要协议 ──────────────────────────────────

   CFG 的数据流分析（liveness、SSA、reaching-def 等）需要知道
   \"这条语句读了哪些变量、定义了哪些变量\"。但不同前端的 AST
   节点形态不同——IRStmt 的 :variable、JSON AST 的 {:type \"var\"}、
   Python AST 的 Name 节点——不能共用。

   协议把这些差异封装起来——数据流算法只调协议方法，不读 AST 字段。

   ── use / def 的边界 ────────────────────────────────

   use  —— 语句执行时读取的变量
   def  —— 语句执行时产生新值的变量

   注意 aset(target=arr, ...) 的特殊性：
   ▸ arr 出现在 target 位置——**既是 use 也是 def**
   ▸ use 语义：读旧 arr 状态作为修改基础
   ▸ def 语义：产生新 arr 状态

   这个双重身份在 SSA 重命名时通过两个方法分别处理：
   stmt-replace-uses 替换成旧版本，stmt-replace-defs 替换成新版本。

   ── 与终结指令的分离 ────────────────────────────────

   stmt 和 term 分开——终结指令（branch / return / throw）不在
   基本块的 stmts 里，只在 terminator 里。它们也有 uses 但无 defs
   （终结指令不产生新值）。

   ── expr 与 stmt 的分离 ─────────────────────────────

   expr-uses / expr-replace-uses 处理表达式子节点。stmt 实现内部
   用它们——比如 assign 的 :val 是表达式，用 expr-uses 提取。
   外部调用方一般只用 stmt / term 级别的方法。"
  )

(defprotocol IVarAccess
  ;; ── 表达式级 ──
  (expr-uses [this expr]
    "表达式 expr 里被读取的变量名集合。返回 #{symbol ...}。

     递归遍历子节点——:variable 节点的 :name 入集合，
     :literal 不入，其他节点递归 children。")

  (expr-replace-uses [this expr subst]
    "按 subst {old-sym → new-sym} 替换表达式里的 variable 节点名。
     返回新表达式。

     只替换 :variable 节点——不修改其他节点的字段。")

  ;; ── 语句级 ──
  (stmt-uses [this stmt]
    "语句 stmt 里被读取的变量名集合。

     ▸ :assign   —— :val 的 uses + 复合左值 :var 的内部 uses
     ▸ :var-decl —— :val 的 uses（:name 是 def，不算 use）
     ▸ :aset     —— :target 的 uses + :idx 的 uses + :val 的 uses
     ▸ 其他语句  —— expr-uses 整条语句")

  (stmt-defs [this stmt]
    "语句 stmt 定义的变量名集合。

     ▸ :assign   —— 简单变量时 #{(:name :var)}，复合左值时 #{}
     ▸ :var-decl —— #{(:name stmt)}
     ▸ :aset     —— 简单变量 target 时 #{(:name :target)}，复合左值时 #{}
     ▸ 其他      —— #{}")

  (stmt-replace-uses [this stmt subst]
    "按 subst 替换 stmt 里 uses 位置的名字。返回新 stmt。

     ▸ :assign   —— 替换 :val 里的 uses；复合左值 :var 也替换内部 uses
     ▸ :var-decl —— 替换 :val 里的 uses
     ▸ :aset     —— 替换 :target（旧版本）、:idx、:val 里的 uses
     ▸ 其他      —— expr-replace-uses 整条语句")

  (stmt-replace-defs [this stmt subst]
    "按 subst 替换 stmt 里 def 位置的名字。返回新 stmt。

     ▸ :assign   —— 简单变量时替换 :var 的 :name
     ▸ :var-decl —— 替换 :name
     ▸ :aset     —— 简单变量 target 时替换 :target 的 :name
     ▸ 其他      —— 原样返回")

  ;; ── 终结指令级 ──
  (term-uses [this term]
    "终结指令里的 uses。

     ▸ :jump   —— #{}
     ▸ :branch —— :test 的 uses
     ▸ :return —— :value 的 uses
     ▸ :throw  —— :value 的 uses")

  (term-replace-uses [this term subst]
    "按 subst 替换终结指令里的 uses。返回新 term。")

  (make-variable-ref [this sym]
    "构造一个变量引用节点——phi 的 :value 需要前端特定的 variable
     节点，但 rename-variables 只知道 symbol 版本名。"))

;; ═══════════════════════════════════════════
;; 基于协议的通用辅助——所有前端共享
;; ═══════════════════════════════════════════

(defn block-uses-defs
  "汇总一个基本块的所有 uses / defs。

   uses —— 块内被读、且读之前未在块内定义的变量。
             遍历 stmts 时维护\"已定义\"集合——先算 use 再更新 defined。
   defs —— 块内被定义的所有变量（包括多次定义）。

   终结指令的 uses 也计入——branch 的 test / return 的 value
   在块末尾被读。

   返回 {:uses #{...} :defs #{...}}。"
  [adapter block]
  (let [stmts    (:stmts block)
        term     (:term block)
        {:keys [uses defs]}
        (reduce (fn [{:keys [uses defs] :as acc} stmt]
                  (let [s-uses (stmt-uses adapter stmt)
                        s-defs (stmt-defs adapter stmt)
                        ;; 块内先读后定义的变量才计入 uses
                        new-uses (reduce (fn [s u]
                                           (if (contains? defs u)
                                             s
                                             (conj s u)))
                                         uses
                                         s-uses)]
                    {:uses new-uses
                     :defs (into defs s-defs)}))
                {:uses #{} :defs #{}}
                stmts)
        term-u   (term-uses adapter term)]
    {:uses (reduce (fn [s u]
                     (if (contains? defs u) s (conj s u)))
                   uses
                   term-u)
     :defs defs}))