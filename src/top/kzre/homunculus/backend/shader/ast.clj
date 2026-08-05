(ns top.kzre.homunculus.backend.shader.ast
  (:require [top.kzre.homunculus.core.ast :refer [defast]]))

(declare ShaderAST kind children reduce-children node-meta
        ->Literal ->Variable ->BinaryOp ->UnaryOp ->Call ->MemberAccess
        ->ArrayIndex ->Constructor ->Cast ->Swizzle
        ->Assign ->VarDecl ->If ->While ->Block
        ->Function ->EntryPoint ->Struct ->ResourceDecl ->Import
        ->Param ->StructMember)

(defast ShaderAST
        {:expr            [:literal / :variable / :binary-op / :unary-op / :call
                           :member-access / :array-index / :constructor / :cast]
         :exprs           [:expr {:many true}]
         :stmt            [:assign / :var-decl / :if / :while / :block / :expr]
         :stmts           [:stmt {:many true}]
         :params          [:param {:many true}]
         :struct-members  [:struct-member {:many true}]}

        ;; 表达式
        :literal         ['val]
        :variable        ['name]
        :binary-op       ['op :expr 'left :expr 'right]
        :unary-op        ['op :expr 'expr]
        :call            ['fn :exprs 'args]
        :member-access   [:expr 'target 'member]
        :array-index     [:expr 'target :expr 'index]
        :constructor     ['type :exprs 'args]
        :cast            ['type :expr 'expr]


        ;; 语句
        :var-decl        ['name 'type :expr 'init {:optional true}]
        :assign          [:expr 'lhs :expr 'rhs]
        :if              [:expr 'test :block 'then :block 'else {:optional true}]
        :while           [:expr 'test :block 'body]
        :block           [:stmts 'stmts :expr 'ret  {:optional true} ]

        ;; 声明
        :function        ['name  'return-type :params 'params :block 'body]
        :entry-point     ['name 'stage 'return-type :params 'params :block 'body]
        :struct          ['name :struct-members 'members]
        :resource-decl   ['name 'resource-kind 'slot :struct-members 'members ]
        :import          ['path]

        ;; 辅助
        :param           ['name 'type]
        :struct-member   ['name 'type])