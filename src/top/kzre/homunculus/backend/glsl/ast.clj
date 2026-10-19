(ns top.kzre.homunculus.backend.glsl.ast
        "GLSL 3.30 core 的 AST。与 IR AST 保持结构对齐——表达式、语句、
         顶层声明的节点划分和字段顺序一致，只增加 GLSL 特有的顶层声明节点。

         ── 与 IR AST 的差异 ──────────────────────────────────
         新增：
           :input-decl         —— GLSL 的 in 变量
           :output-decl        —— GLSL 的 out 变量
           :interface-block    —— GLSL 的接口块（in/out 的另一种形态）
           :global-var         —— 对应 IR 的 :static-var（GLSL 无 static 关键字）
           :uniform-block      —— 对应 IR 的 :cbuffer-decl

         消失（被合并或展开）：
           :entry-point                  → 降级为 :function name=\"main\"
           :texture-decl + :sampler-decl → :uniform（类型为 sampler2D 等）
           :cbuffer-decl                 → :uniform-block
           :import                       → 编译期处理，AST 层无

         ── 接口块与独立 in/out 的关系 ────────────────────────
         两者可以在同一 shader 中共存——各自占用独立的槽位。链接时接口块
         按块名匹配，独立变量按变量名匹配；接口块不能与独立变量交叉匹配。

         ── 字段约定 ────────────────────────────────────────────
         'name          标识符（字符串）
         'instance-name 接口块实例名（字符串）
         'type          GLSL 类型名（字符串，含数组形式如 \"float[4]\"）
         'location      in/out / 接口块 / 成员的 layout(location=N)，
                        nil 表示不写
         'flat?         整数插值是否需要 flat
         'binding       uniform block 的 layout(binding=N)，nil 表示不写
         'packing       uniform block 的打包方式——:std140 / :std430
         'const?        global-var 是否为 const——false 时不写修饰符
         'val           字面量值（Clojure 数字/布尔）
         'op            操作符关键字
         'fn            函数名
         'member        成员名
         'lhs/'rhs      赋值左右值"
        (:require [top.kzre.homunculus.core.ast :refer [defast]]))

(declare GlslAST kind reduce-children
        ->Literal ->Variable ->BinaryOp ->UnaryOp ->Call
        ->MemberAccess ->ArrayIndex ->Constructor ->Cast
        ->Assign ->VarDecl ->If ->While ->Block
        ->Uniform ->GlobalVar
        ->InputDecl ->OutputDecl ->UniformBlock ->InterfaceBlock
        ->Function ->Struct
        ->Param ->StructMember ->InterfaceBlockMember
        map->Literal map->Variable map->BinaryOp map->UnaryOp map->Call
        map->MemberAccess map->ArrayIndex map->Constructor map->Cast
        map->Assign map->VarDecl map->If map->While map->Block
        map->Uniform map->GlobalVar
        map->InputDecl map->OutputDecl map->UniformBlock map->InterfaceBlock
        map->Function map->Struct
        map->Param map->StructMember map->InterfaceBlockMember)

(defast GlslAST
        {:expr                  [:literal / :variable / :binary-op / :unary-op / :call /
                                 :member-access / :array-index / :constructor / :cast]
         :exprs                 [:expr {:many true}]
         :stmt                  [:assign / :var-decl / :if / :while / :block /
                                 :uniform / :global-var /
                                 :input-decl / :output-decl /
                                 :uniform-block / :interface-block /
                                 :function / :struct]
         :stmts                 [:stmt {:many true}]
         :params                [:param {:many true}]
         :struct-members        [:struct-member {:many true}]
         :interface-block-members [:interface-block-member {:many true}]}

        ;; ── 表达式（完全对齐 IR） ──────────────────────
        :literal         ['val 'attrs]
        :variable        ['name 'attrs]
        :binary-op       ['op :expr 'left :expr 'right 'attrs]
        :unary-op        ['op :expr 'expr 'attrs]
        :call            ['fn :exprs 'args 'attrs]
        :member-access   [:expr 'target 'member 'attrs]
        :array-index     [:expr 'target :expr 'index 'attrs]
        :constructor     ['type :exprs 'args 'attrs]
        :cast            ['type :expr 'expr 'attrs]

        ;; ── 语句 ──────────────────────────────────────
        :var-decl        ['name 'type :expr 'init {:optional true} 'attrs]
        :uniform         ['name 'type 'attrs]
        :global-var      ['name 'type :expr 'init {:optional true} 'const? 'attrs]
        :input-decl      ['name 'location 'type 'flat? 'attrs]
        :output-decl     ['name 'location 'type 'flat? 'attrs]
        :uniform-block   ['name 'binding 'packing :struct-members 'members 'attrs]

        ;; ── 接口块（in/out 的块形态） ─────────────────
        ;; 块级 location 可选——作为块内成员连续分配的起点。
        ;; 成员级 location 覆盖块级设置，并成为后续成员的新起点。
        :interface-block ['name  'direction 'instance-name 'location
                          :interface-block-members 'members
                          'attrs]

        :assign          [:expr 'lhs :expr 'rhs 'attrs]
        :if              [:expr 'test :block 'then :block 'else {:optional true} 'attrs]
        :while           [:expr 'test :block 'body 'attrs]
        :block           [:stmts 'stmts :expr 'ret {:optional true} 'attrs]

        ;; ── 顶层 ──────────────────────────────────────
        :function        ['name 'return-type :params 'params :block 'body 'attrs]
        :struct          ['name :struct-members 'members 'attrs]

        ;; ── 辅助 ──────────────────────────────────────
        :param           ['name 'type 'attrs]
        :struct-member   ['name 'type 'attrs]
        :interface-block-member ['name 'location 'type 'flat? 'attrs])