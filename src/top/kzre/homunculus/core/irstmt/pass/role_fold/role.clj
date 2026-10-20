(ns top.kzre.homunculus.core.irstmt.pass.role-fold.role
  "角色模型定义。

   角色表是 analyze 的输出契约、transform 的输入契约。
   analyze 负责填充，transform 负责消费，两者只共享本文件的 record 定义。

   ══════════════════════════════════════════════════════════════
   背景：角色识别要解决什么问题
   ══════════════════════════════════════════════════════════════

   序列函数（map / filter / reduce 等）在 lowering 后会展开成
   `while + recur_p + ifval` 的机械结构。这种结构在 shader 语言里
   会产生大量冗余的数组分配和拷贝：

     float arr[4];                        // 积累器（accumulator）
     int i = 0;                           // 循环计数器
     float result[4];                     // 收集器（collector）
     while (recur) {
         if (i < COUNT) {
             arr[i] = f(...);             // 积累写入
             i++;
         } else {
             result[0] = arr[0];          // 收集拷贝
             result[1] = arr[1];
             ...
         }
     }
     float y[4] = result;                 // 接收者（receiver）

   同一份数据在三个时点被三份数组承载，每次转移都是全量拷贝。
   角色识别把这些\"同一份数据的多个名字\"识别出来，让 transform
   把多份存储压成一份，删除中间拷贝。

   同理 if 表达式会展开成：

     int t;                               // if-收集器
     if (cond) { t = v1; } else { t = v2; }
     int ll = t;                          // if-接收者

   `t` 是 if 表达式的 phi 节点，`ll` 是使用者。

   ══════════════════════════════════════════════════════════════
   两条角色链
   ══════════════════════════════════════════════════════════════

   数组链：
     LoopCounter ──索引──▶ Accumulator ──拷贝──▶ Collector ──拷贝──▶ Receiver

     - LoopCounter 提供索引，驱动 Accumulator 逐槽位写入
     - Accumulator 是循环体内的\"写入目标\"，累积中间结果
     - Collector 在循环结束后从 Accumulator 全量拷贝
     - Receiver 是 Collector 的拷贝接收者，也是最终消费者

   识别后，transform 可以把 Receiver 声明为唯一数组，
   删除 Accumulator 的声明、把累加写入重定向到 Receiver、
   删除 Collector 的全量拷贝。三份存储变一份。

   if 链：
     IfCollector ──拷贝──▶ IfReceiver

     - IfCollector 在 if 的 then/else 两个分支都被赋值，
       等价于 if 表达式的 phi 节点
     - IfReceiver 拷贝 IfCollector 的值并使用

   识别后，transform 可以删除 IfCollector 的声明、
   把两个分支的赋值目标改为 IfReceiver、删除接收赋值。

   ══════════════════════════════════════════════════════════════
   识别与变换的分离
   ══════════════════════════════════════════════════════════════

   - analyze 只读：扫描 IR，填充角色表，不改 IR
   - transform 只写：消费角色表，修改 IR，不做识别
   - 两者只通过本文件的 record 定义交互

   每个角色都记录其源节点引用（while-node / if-node / aset-nodes /
   var-decl-node），transform 据此直接定位要删除或替换的节点，
   不需要重新扫描 IR。

   字段中不冗余存储可从节点推导的信息（如元素类型、初始值），
   需要时从源节点上读取。")

;; ═══════════════════════════════════════════
;; 数组链角色
;; ═══════════════════════════════════════════

(defrecord LoopCounter
  [var-name      ; 循环计数器变量名
   while-node])  ; 所属 :while 节点
;; 循环计数器：驱动 Accumulator 逐槽位写入的索引变量。
;;
;; 判定条件（analyze 负责验证，此处仅描述语义）：
;;   1. var-decl(i, 常量) 位于 while 之前
;;   2. while 体内有 assign(i, i + 常量) 或 assign(i, 常量)
;;   3. i 参与 while 体内的某个 if 测试（形式 i < COUNT 或 i <= COUNT）
;;   4. i 在 while 内没有其他形式的修改
;;
;; COUNT 可以是常量也可以是变量——识别只关心\"i 是计数器\"，
;; 不关心循环长度。这是序列函数场景的核心需求（循环长度不定长）。
;;
;; 例：
;;   int i = 0;                     ← var-decl
;;   while (recur) {
;;       if (i < COUNT) {           ← i 参与 test
;;           arr[i] = f(...);       ← i 作为索引
;;           i = i + 1;             ← 自增
;;       }
;;   }
;;
;; 不属于任何角色链的消费端——LoopCounter 本身不被消除，
;; 它的存在是为了让 Accumulator 的识别能够确定\"索引是计数器\"。

(defrecord Accumulator
  [var-name      ; 数组变量名（accumulator）
   while-node    ; 所属 :while
   counter-name]) ; 索引用的循环计数器变量名
;; 积累器：在循环体内被逐槽位写入的数组。
;;
;; 判定条件：
;;   1. var-decl(arr, TVec) 位于 while 之前
;;   2. arr 在 while 体内的所有 aset 写入：
;;        - 目标为 (variable arr)
;;        - 索引为某个已识别的 LoopCounter
;;   3. arr 在 while 体内的读取仅为常量索引的 aget，
;;      且只出现在 while 的退出分支（else 分支）
;;   4. arr 在 while 之外没有被读取（除了作为 Collector 的拷贝源）
;;
;; 条件是\"写入用计数器索引\"，与 COUNT 的具体值无关。
;; 循环次数不定长完全不影响识别。
;;
;; 例：
;;   float arr[4];                  ← var-decl（累积器声明）
;;   int i = 0;
;;   while (...) {
;;       if (i < n) {
;;           arr[i] = f(x[i]);      ← 累积写入，索引是 i
;;           i++;
;;       } else {
;;           result[k] = arr[k];    ← 退出分支读取（允许）
;;       }
;;   }
;;
;; transform 处理 Accumulator 时：
;;   - 若该 Accumulator 有对应的 Collector+Receiver 链，
;;     把所有 arr[i] = e 重定向为 receiver[i] = e，
;;     然后删除 arr 的声明。
;;   - 若没有 Collector（Accumulator 直接是最终结果），
;;     保留不动（本系统不做优化）。

(defrecord Collector
  [var-name      ; 收集器数组名
   source        ; 源 accumulator 变量名
   indices       ; 常量索引集合 #{0 1 2 3 ...}，必须完整覆盖 [0, N-1]
   aset-nodes    ; 对应的 :aset 节点列表（transform 据此删除）
   var-decl-node]) ; 收集器声明节点（transform 据此删除）
;; 收集器：从 Accumulator 全量拷贝的中间数组。
;;
;; 判定条件：
;;   1. var-decl(col, TVec) 与 Accumulator 在同一个 Block
;;   2. col 的所有 aset 写入都是 col[c] = (aget arr c)，
;;      c 是常量，indices 恰好覆盖 [0, N-1] 全部（无遗漏无重复）
;;   3. 这些写入位于同一个 while 的退出分支或 while 之后
;;   4. col 没有其他写入
;;   5. source 必须对应某个已识别的 Accumulator
;;
;; \"indices 完整覆盖\"是关键条件——部分拷贝（比如只拷 0..2 缺 3）
;; 意味着 Collector 不是 Accumulator 的完整快照，可能是选择性提取，
;; 这种情况放弃识别。
;;
;; 例：
;;   while (...) {
;;       if (i < n) { ... }
;;       else {
;;           result[0] = arr[0];    ← collector 的 aset
;;           result[1] = arr[1];    ← 索引集合 #{0,1,2,3}
;;           result[2] = arr[2];
;;           result[3] = arr[3];
;;       }
;;   }
;;
;; aset-nodes 直接持有要删除的节点引用——
;; transform 拿到 Collector 就能批量删除，不需要重扫 IR。
;; var-decl-node 用于删除 Collector 的声明。
;;
;; transform 处理 Collector 时：
;;   - 删除所有 aset-nodes
;;   - 删除 var-decl-node
;;   - 若有 Receiver，把 col 的所有引用替换为 receiver 变量名
;;   - 若无 Receiver，把 col 的引用替换为 source（Accumulator）

(defrecord Receiver
  [var-name      ; 接收者数组名
   source        ; 源 collector 变量名
   var-decl-node]) ; 接收者声明节点（init 是 (variable source)）
;; 接收者：从 Collector 整体赋值的最终数组。
;;
;; 判定条件：
;;   1. var-decl(recv, init = (variable col))，col 是已识别的 Collector
;;   2. recv 在作用域内没有后续 aset 写入（只被读取或整体传递）
;;
;; Receiver 是\"数据的最终归宿\"——它是 Accumulator 数据的
;; 真正消费者。transform 时 Receiver 保留声明（成为新的 Accumulator），
;; 只是把 init 置 nil（因为数据已经直接累积到它里面了）。
;;
;; 例：
;;   float y[4] = result;           ← receiver 的 var-decl，init 是 result
;;   use(y);                        ← 后续使用 y
;;
;; transform 处理 Receiver 时：
;;   - var-decl 的 init 置 nil（数据已从 Accumulator 重定向到这里）
;;   - 保留 Receiver 的声明和类型——它是新的数据容器
;;   - Receiver 的变量名成为 Accumulator 写入的新目标

;; ═══════════════════════════════════════════
;; if 链角色
;; ═══════════════════════════════════════════

(defrecord IfCollector
  [var-name       ; 收集器变量名（标量）
   if-node        ; 所属 :if 节点
   then-assign    ; then 分支里的 :assign 节点
   else-assign    ; else 分支里的 :assign 节点
   var-decl-node]) ; 收集器声明节点
;; if-收集器：在 if 的 then 和 else 两个分支都被赋值的标量变量。
;;
;; 语义上等价于 if 表达式的 phi 节点：
;;   (if cond v1 v2)  ⟶  t = if (cond) { v1 } else { v2 }
;;
;; 判定条件：
;;   1. var-decl(t) 在 if 之前
;;   2. t 在 if 的 then 和 else 两个分支都被赋值
;;      （assign(t, ...) 或 assign(t, t2)，其中 t2 是嵌套的 IfCollector）
;;   3. 每个分支内 t 只有一次赋值
;;
;; 注意：不检查 if 的 test 是否为常量。
;; `if (true)` / `if (x > 0)` / `if (false)` 都识别——
;; 识别条件是\"两个分支都给 t 赋值\"这个结构，与 test 值无关。
;; 常量 if 的折叠留给其他 pass 处理，本系统不与常量折叠耦合。
;;
;; 例：
;;   int t;                         ← var-decl
;;   if (x > 0) {
;;       t = 1;                     ← then-assign
;;   } else {
;;       int t2;                    ← 嵌套的 IfCollector
;;       if (y > 0) { t2 = 2; } else { t2 = 3; }
;;       t = t2;                    ← else-assign（源是 t2）
;;   }
;;   int ll = t;                    ← t 被 IfReceiver 消费
;;
;; then-assign / else-assign 直接持有赋值节点引用，
;; transform 删除这两个 assign 后，把 t 的所有 use 替换为 Receiver。
;;
;; 嵌套情形：`t = t2` 中 t2 也是 IfCollector 时，
;; analyze 会递归识别 t2 为独立的 IfCollector，
;; transform 分两轮处理：
;;   第一轮：处理最内层 if，把 t2 的分支赋值改为 t 的分支赋值
;;   第二轮：处理外层，把 t 的赋值改为 Receiver 的赋值
;; 但更简单的做法是：analyze 时把嵌套 IfCollector 展开——
;; transform 一次性把所有分支的赋值目标替换为最终 Receiver。

(defrecord IfReceiver
  [var-name       ; 接收者变量名
   source         ; 源 if-collector 变量名
   var-decl-node]) ; 接收者声明节点
;; if-接收者：从 IfCollector 整体赋值的最终变量。
;;
;; 判定条件：
;;   1. var-decl(r, init = (variable t))，t 是已识别的 IfCollector
;;   2. r 在作用域内没有后续重新赋值
;;
;; 例：
;;   int ll = t;                    ← r = ll, source = t
;;   use(ll);                       ← 后续使用 ll
;;
;; transform 处理 IfReceiver 时：
;;   - var-decl 的 init 置 nil（数据已直接从分支赋值到 ll）
;;   - 保留 ll 的声明
;;   - ll 成为 if 分支的新赋值目标

;; ═══════════════════════════════════════════
;; 角色表
;; ═══════════════════════════════════════════

(defrecord RoleTable
  [loop-counters   ; {var-name → LoopCounter}
   accumulators    ; {var-name → Accumulator}
   collectors      ; {var-name → Collector}
   receivers       ; {var-name → Receiver}
   if-collectors   ; {var-name → IfCollector}
   if-receivers])  ; {var-name → IfReceiver}
;; 角色表：一次 analyze 的完整产出。
;;
;; 每个字段是以变量名为 key 的 map——同一变量名在 IRStmt 里
;; 由 statementize 保证唯一（gensym），不会跨 Block 冲突。
;;
;; 字段之间的依赖关系（analyze 填充顺序）：
;;   loop-counters → accumulators → collectors → receivers
;;   if-collectors → if-receivers
;;
;; transform 消费时的顺序（逆依赖）：
;;   receivers → collectors → accumulators
;;   if-receivers → if-collectors
;;
;; 表为空（table-empty? 返回 true）时，role/core 的迭代终止。

(defn empty-table
  "空角色表。analyze 的初始值。"
  []
  (->RoleTable {} {} {} {} {} {}))

(defn table-empty?
  "角色表是否完全没有识别到任何角色。
   analyze 返回空表时，role/core 的迭代终止。"
  [table]
  (and (empty? (:loop-counters table))
       (empty? (:accumulators table))
       (empty? (:collectors table))
       (empty? (:receivers table))
       (empty? (:if-collectors table))
       (empty? (:if-receivers table))))

;; ── 添加（analyze 使用）──

(defn add-loop-counter [table lc]
  (assoc-in table [:loop-counters (:var-name lc)] lc))

(defn add-accumulator [table acc]
  (assoc-in table [:accumulators (:var-name acc)] acc))

(defn add-collector [table col]
  (assoc-in table [:collectors (:var-name col)] col))

(defn add-receiver [table rcv]
  (assoc-in table [:receivers (:var-name rcv)] rcv))

(defn add-if-collector [table ifc]
  (assoc-in table [:if-collectors (:var-name ifc)] ifc))

(defn add-if-receiver [table ifr]
  (assoc-in table [:if-receivers (:var-name ifr)] ifr))

;; ── 查询（transform 使用）──

(defn loop-counter [table var-name]
  (get-in table [:loop-counters var-name]))

(defn accumulator [table var-name]
  (get-in table [:accumulators var-name]))

(defn collector [table var-name]
  (get-in table [:collectors var-name]))

(defn receiver [table var-name]
  (get-in table [:receivers var-name]))

(defn if-collector [table var-name]
  (get-in table [:if-collectors var-name]))

(defn if-receiver [table var-name]
  (get-in table [:if-receivers var-name]))

;; ── 谓词 ──

(defn loop-counter? [table var-name]
  (contains? (:loop-counters table) var-name))

(defn accumulator? [table var-name]
  (contains? (:accumulators table) var-name))

(defn collector? [table var-name]
  (contains? (:collectors table) var-name))

(defn receiver? [table var-name]
  (contains? (:receivers table) var-name))

(defn if-collector? [table var-name]
  (contains? (:if-collectors table) var-name))

(defn if-receiver? [table var-name]
  (contains? (:if-receivers table) var-name))

;; ── 合并（多作用域合并时用）──

(defn merge-tables
  "合并两个角色表。同名变量后者覆盖前者。
   用于 analyze 在嵌套作用域中累积角色（如果将来需要跨 Block 分析）。"
  [t1 t2]
  (->RoleTable
    (merge (:loop-counters t1) (:loop-counters t2))
    (merge (:accumulators  t1) (:accumulators  t2))
    (merge (:collectors    t1) (:collectors    t2))
    (merge (:receivers     t1) (:receivers     t2))
    (merge (:if-collectors t1) (:if-collectors t2))
    (merge (:if-receivers  t1) (:if-receivers  t2))))

;; ── 调试辅助 ──

(defn table-summary
  "角色表摘要——每个角色链的变量名列表。用于测试断言和调试输出。

   单元测试断言 analyze 的输出时，直接用 table-summary 比对——
   不关心节点引用（节点是 AST 实例，难以直接比较），只看
   \"识别了哪些变量名作为什么角色\"。这让测试聚焦在识别正确性
   而非结构细节上。"
  [table]
  {:loop-counters (sort (keys (:loop-counters table)))
   :accumulators  (sort (keys (:accumulators  table)))
   :collectors    (sort (keys (:collectors    table)))
   :receivers     (sort (keys (:receivers     table)))
   :if-collectors (sort (keys (:if-collectors table)))
   :if-receivers  (sort (keys (:if-receivers  table)))})