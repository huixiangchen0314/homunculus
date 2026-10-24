(ns top.kzre.homunculus.core.cfg.protocol
  "CFG 的协议定义。

   分层：
   ▸ IBasicBlock —— 基本块：id / stmts / term / origin / successors
   ▸ ITerminator —— 终结指令：kind / successors

   CFG 本身是普通 record（见 model），用函数操作——
   不需要协议，因为只有一种实现，且操作语义独立于多态。

   设计要点：
   - 边的唯一真相源是 terminator 的 target/then/else——
     CFG 不存显式的边集合，避免 term 和边索引不一致。
   - preds 索引按需构建（cfg-build-preds），不做缓存——
     shader 函数规模小（通常 < 100 块），O(n) 遍历可接受。
   - 没有虚拟 exit 块——终止块（:return / :throw）由
     cfg-terminators 派生。这避免额外的虚拟边，数据流分析的
     后向边界就是这些终止块。")

(defprotocol IBasicBlock
  (bb-id [this]
    "块的唯一标识。keyword / long / symbol 均可，CFG 内部用作 map key。")
  (bb-stmts [this]
    "非终结语句序列（IRStmt 的 stmt 类节点：assign / var-decl / aset / ...）。
     不能包含 :if / :while / :block——这些通过 CFG 的边结构表达。")
  (bb-term [this]
    "终结指令（ITerminator 实例）。")
  (bb-origin [this]
    "源 AST 节点（IRStmt 的 :if / :while / :block 节点）——CFG → IRStmt
     重建时的锚点。nil 表示该块由多个顺序 AST 语句拼接产生，或是入口块。")
  (bb-successors [this]
    "后继块 id 列表。等价于 (term-successors (bb-term this))——便捷方法。"))

(defprotocol ITerminator
  (term-kind [this]
    "终结类型：
       :jump   —— 无条件跳转到 target
       :branch —— 条件跳转，test 为真走 then，否则走 else
       :return —— 函数返回，value 为返回表达式（nil 表示 void）
       :throw  —— 抛出异常，value 为异常表达式")
  (term-successors [this]
    "后继块 id 列表。
       :jump   → [target]
       :branch → [then else]
       :return → []  （终止块）
       :throw  → []  （终止块）"))