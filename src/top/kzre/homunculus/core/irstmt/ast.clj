(ns top.kzre.homunculus.core.irstmt.ast
        "语句化 IR，仅移除 let/binding/loop/recur/lambda，其余节点原样保留。
         expr 与 stmt 在类型映射中显式分离。"
        (:require [top.kzre.homunculus.core.ast :refer [defast]]))

(declare IRStmt kind children reduce-children rreduce-children node-meta
        ->Literal ->Variable ->Call ->If ->Block
        ->VarDecl ->Vector ->Map ->Try ->Catch ->Throw ->Assign ->While ->Convert
        ->Ns ->Method ->Field ->ProtocolImpl ->Record ->Protocol ->MemberAccess
        ->NewArray ->Aget ->Aset ->Alength ->Param ->Pair ->Binding
        ->Function
        map->Literal map->Variable map->Call map->If map->Block
        map->VarDecl map->Vector map->Map map->Try map->Catch map->Throw map->Assign map->While map->Convert
        map->Ns map->Method map->Field map->ProtocolImpl map->Record map->Protocol map->MemberAccess
        map->NewArray map->Aget map->Aset map->Alength map->Param map->Pair)

(defn attrs
  [node]
  (:attrs node))

(defast IRStmt
        {:expr           [:literal / :variable / :call / :if / :block   ;; 纯表达式
                          :member-access / :vector / :map / :new-array
                          :aget / :aset / :alength]
         :stmt           [:var-decl / :assign / :while / :convert      ;; 语句
                          :try / :catch / :throw
                          :record / :protocol / :ns]                    ;; 顶层/类型节点也视为语句
         :exprs          [:expr {:many true}]
         :stmts          [:stmt {:many true}]
         :params         [:param {:many true}]
         :pairs          [:pair {:many true}]
         :catches        [:catch {:many true}]
         :record-fields  [:field {:many true}]
         :protocol-methods [:method {:many true}]
         :protocol-impls [:protocol-impl {:many true}]}

        ;; 表达式 (字段顺序、attrs 位置与 IR2 一致)
        :literal         ['val 'attrs]
        :variable        ['name 'attrs]
        :call            [:expr 'fn :exprs 'args 'attrs]
        :if              [:expr 'test :expr 'then :expr 'else {:optional true} 'attrs]
        :block           [:stmts 'stmts :expr 'ret {:optional true} 'attrs]  ;; 语句列表 + 可选 ret

        :function        ['name :params 'params :block 'body 'attrs]
        :var-decl        ['name :expr 'val {:optional true} 'docstring 'attrs]
        :vector          [:exprs 'items 'attrs]
        :map             [:pairs 'pairs 'attrs]
        :assign          [:expr 'var :expr 'val 'attrs]
        :while           [:expr 'test :stmt 'body 'attrs]
        :convert         [:expr 'expr 'src-ty 'dst-ty 'attrs]
        :member-access   [:expr 'target 'accessor :exprs 'args 'attrs]
        :new-array       [:expr 'size 'attrs]
        :aget            [:expr 'target :expr 'idx 'attrs]
        :aset            [:expr 'target :expr 'idx :expr 'val 'attrs]
        :alength         [:expr 'target 'attrs]

        ;; 辅助
        :param           ['name 'attrs]
        :pair            [:expr 'key :expr 'val 'attrs]
        :catch           ['class 'sym :stmt 'body 'attrs]
        :field           ['name 'attrs]
        :method          ['name :params 'params :stmt 'body {:optional true} 'docstring 'attrs]
        :protocol-impl   ['proto-name :protocol-methods 'methods 'attrs]
        :record          ['name :record-fields 'fields :protocol-impls 'protocols 'attrs]
        :protocol        ['name :protocol-methods 'methods 'attrs]
        :ns              ['name 'requires 'docstring 'attrs]
        :try             [:stmt 'body {:optional true} :catches 'catches :stmt 'finally {:optional true} 'attrs]
        :throw           [:expr 'expr 'attrs])
