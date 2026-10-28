(ns top.kzre.homunculus.frontend.shader.ast
  (:require [top.kzre.homunculus.core.ast :refer [defast]]))

(declare ShaderAST kind children reduce-children node-meta
        ->Literal ->Variable ->BinaryOp ->UnaryOp ->Call ->MemberAccess
        ->ArrayIndex ->Constructor ->Cast
        ->Assign ->VarDecl ->If ->While ->For ->Block
        ->Function ->EntryPoint ->Struct ->Import
        ->CbufferDecl ->TextureDecl ->SamplerDecl
        ->Param ->StructMember
        ->Uniform ->StaticVar)

(defn attrs[node] (:attrs node))

(defast ShaderAST
        {:expr            [:literal / :variable / :binary-op / :unary-op / :call /
                           :member-access / :array-index / :constructor / :cast]
         :exprs           [:expr {:many true}]
         :stmt            [:assign / :var-decl / :if / :while / :for / :block / :expr /
                           :uniform / :static-var /
                           :texture-decl / :sampler-decl / :cbuffer-decl]
         :stmts           [:stmt {:many true}]
         :params          [:param {:many true}]
         :struct-members  [:struct-member {:many true}]}

        ;; 表达式
        :literal         ['val  'attrs]
        :variable        ['name  'attrs]
        :binary-op       ['op :expr 'left :expr 'right 'attrs]
        :unary-op        ['op :expr 'expr 'attrs]
        :call            ['fn :exprs 'args 'attrs]
        :member-access   [:expr 'target 'member 'attrs]
        :array-index     [:expr 'target :expr 'index 'attrs]
        :constructor     ['type :exprs 'args 'attrs]
        :cast            ['type :expr 'expr 'attrs]

        ;; 语句
        :var-decl        ['name 'type :expr 'init {:optional true} 'attrs]
        :uniform         ['name 'type 'attrs]
        :static-var      ['name 'type :expr 'init {:optional true} 'attrs]
        :assign          [:expr 'lhs :expr 'rhs 'attrs]
        :if              [:expr 'test :block 'then :block 'else {:optional true} 'attrs]
        :while           [:expr 'test :block 'body 'attrs]
        :for             [:stmt 'init {:optional true}
                          :expr 'test
                          :stmt 'update {:optional true}
                          :block 'body
                          'attrs]
        :block           [:stmts 'stmts :expr 'ret  {:optional true} 'attrs]

        ;; 声明
        :function        ['name 'return-type
                          :params 'params
                          :block 'body
                          'attrs]
        :entry-point     ['name 'stage 'return-type
                          :params 'params
                          :block 'body
                          'attrs]
        :struct          ['name :struct-members 'members 'attrs]

        :texture-decl    ['name 'register 'dimension 'element-type 'attrs]
        :sampler-decl    ['name 'register 'compare? 'attrs]
        :cbuffer-decl    ['name 'register :struct-members 'members 'attrs]
        :import          ['path 'attrs]

        ;; 辅助
        :param           ['name 'type 'attrs]
        :struct-member   ['name 'type 'attrs])

