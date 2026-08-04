(ns top.kzre.homunculus.backend.shader.ast
  (:require [top.kzre.homunculus.core.ast :refer [defast]]))

(declare ShaderAST)

(defast ShaderAST
        {:expr            [:literal / :variable / :binary-op / :unary-op / :call
                           :member-access / :array-index / :constructor / :cast / :swizzle]
         :stmt            [:assign / :var-decl / :if / :while / :block / :expr]
         :stmts           [:stmt {:many true}]
         :params          [:param {:many true}]
         :struct-members  [:struct-member {:many true}]
         :decls           [:function / :entry-point / :struct / :resource-decl / :import {:many true}]}

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
        :swizzle         [:expr 'base 'mask]

        ;; 语句
        :assign          [:expr 'lhs :expr 'rhs]
        :var-decl        ['name 'type :expr 'init {:optional true}]
        :if              [:expr 'test :block 'then :block 'else {:optional true}]
        :while           [:expr 'test :block 'body]
        :block           [:stmts 'stmts]

        ;; 声明
        :function        ['name  'return-type :params 'params :expr 'ret {:optional true} :block 'body]
        :entry-point     ['name 'stage 'return-type :params 'params :expr 'ret {:optional true} :block 'body]
        :struct          ['name :struct-members 'members]
        :resource-decl   ['name 'resource-kind 'slot :struct-members 'members {:optional true}]
        :import          ['path]

        ;; 辅助
        :param           ['name 'type]
        :struct-member   ['name 'type])