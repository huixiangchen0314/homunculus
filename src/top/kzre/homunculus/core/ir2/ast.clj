(ns top.kzre.homunculus.core.ir2.ast
  "IR2 语言无关 AST 的节点记录定义。所有节点实现 INode 协议。
   使用 reduce-children，每个节点直接处理字段。"
  (:require [top.kzre.homunculus.core.ast :refer [defast]]))

(declare IR2 kind children reduce-children rreduce-children node-meta
        ->Literal ->Variable ->Call ->If ->Block ->Let ->Lambda ->Loop ->Recur
        ->Define ->Vector ->Map ->Try ->Catch ->Throw ->Assign ->While ->Convert
        ->Ns ->Method ->Field ->ProtocolImpl ->Record ->Protocol ->MemberAccess
        ->NewArray ->Aget ->Aset ->Alength ->Param ->Pair ->Binding)

(defn attrs [node]
  (:attrs node))

(defn ir2?
  [node]
  (satisfies? IR2 node))


(defast IR2
        {:expr           [:literal / :variable / :call / :if / :block / :let / :lambda
                          :loop / :recur / :define / :vector / :map
                          :try / :catch / :throw / :assign / :while / :convert
                          :record / :protocol / :member-access / :ns
                          :new-array / :aget / :aset / :alength]
         :exprs          [:expr {:many true}]
         :bindings       [:binding {:many true}]
         :catches        [:catch {:many true}]
         :record-fields  [:field {:many true}]
         :protocol-methods [:method {:many true}]
         :protocol-impls [:protocol-impl {:many true}]
         :params         [:param {:many true}]
         :pairs          [:pair {:many true}]}

        :literal         ['val 'attrs]
        :variable        ['name 'attrs]
        ;; TODO 改名叫做 Symbol
        :symbol          ['name 'attrs]
        :call            [:expr 'fn :exprs 'args 'attrs]
        :if              [:expr 'test :expr 'then :expr 'else {:optional true} 'attrs]
        :block           [:exprs 'exprs 'attrs]
        :let             [:bindings 'bindings :expr 'body 'attrs]
        ;; TODO 移除这里闭包捕获参数， 需要时候本地环境分析更加合适
        :lambda          [:params 'params :expr 'body 'captures 'fn-name 'attrs]
        :loop            [:bindings 'bindings :expr 'body 'attrs]
        :recur           [:exprs 'args 'attrs]
        :define          ['name :expr 'val {:optional true} 'docstring 'attrs]
        :vector          [:exprs 'items 'attrs]
        :map             [:pairs 'pairs 'attrs]
        :try             [:expr 'body {:optional true} :catches 'catches :expr 'finally {:optional true} 'attrs]
        :catch           ['class 'sym :expr 'body 'attrs]
        :throw           [:expr 'expr 'attrs]
        :assign          [:variable 'var :expr 'val 'attrs]
        :while           [:expr 'test :expr 'body 'attrs]
        :convert         [:expr 'expr 'src-ty 'dst-ty 'attrs]
        :ns              ['name 'requires 'docstring 'attrs]
        :method          ['name :params 'params :expr 'body {:optional true} 'docstring 'attrs]
        :field           ['name 'attrs]
        :protocol-impl   ['proto-name :protocol-methods 'methods 'attrs]
        :record          ['name :record-fields 'fields :protocol-impls 'protocols 'attrs]
        :protocol        ['name :protocol-methods 'methods 'attrs]
        :member-access   [:expr 'target 'accessor :exprs 'args 'attrs]
        :new-array       [:expr 'size 'attrs]
        :aget            [:expr 'target :expr 'idx 'attrs]
        :aset            [:expr 'target :expr 'idx :expr 'val 'attrs]
        :alength         [:expr 'target 'attrs]
        :param           ['name 'attrs]
        :pair            [:expr 'key :expr 'val 'attrs]
        :binding         [:variable 'var :expr 'val 'attrs])