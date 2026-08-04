(ns top.kzre.homunculus.core.ir1.ast
  "IR1 AST 节点记录定义。所有节点实现 INode 协议。
   不再包含 children 字段，children 通过协议方法动态返回。
   注意：多表达式体（如 let/loop/fn/try 的 body）会被展开为多个子节点。"
  (:require [top.kzre.homunculus.core.ast :refer [defast]]))


(declare IR1 kind children reduce-children node-meta
  ->Literal ->Symbol ->Vector ->Pair ->Map
  ->Call ->If ->Do ->Binding ->Let ->Param
  ->Fn ->Def ->Loop ->Recur ->Quote ->Var ->Set
  ->Try ->Catch ->Throw ->Ns ->Method ->Field
  ->ProtocolImpl ->Record ->Protocol ->MemberAccess)

(defast IR1
        ;; ── 类型映射 ──
        {:expr           [:literal / :symbol / :call / :if / :do / :let / :fn
                          :def / :loop / :recur / :quote / :var
                          :throw / :try / :catch / :set
                          :vector / :map
                          :record / :protocol / :member-access / :ns]
         :exprs          [:expr {:many true}]
         :bindings       [:binding {:many true}]
         :catches        [:catch {:many true}]
         :record-fields  [:field {:many true}]
         :protocol-methods [:method {:many true}]
         :protocol-impls [:protocol-impl {:many true}]
         :params         [:param {:many true}]
         :pairs          [:pair {:many true}]}

        ;; ── 节点定义（字段顺序严格匹配 ir1.model 记录）──
        :literal         ['val]
        :symbol          ['name]
        :vector          [:exprs 'items]
        :pair            [:expr 'key :expr 'val]
        :map             [:pairs 'pairs]
        :call            ['op :exprs 'args]
        :if              [:expr 'test :expr 'then :expr 'else]
        :do              [:exprs 'exprs]
        :let             [:bindings 'bindings :expr 'body]
        :fn              ['name :params 'params :expr 'body]
        :def             ['name :expr 'val 'docstring]     ; 顺序: name, val, docstring
        :loop            [:bindings 'bindings :expr 'body]
        :recur           [:exprs 'exprs]
        :quote           [:expr 'expr]
        :var             [:symbol 'var-sym]
        :throw           [:expr 'expr]
        :set             [:expr 'var :expr 'val]
        :try             [:expr 'body :catches 'catches :expr 'finally]
        :catch           ['class 'sym :expr 'body]         ; class/sym 属性
        :binding         [:expr 'var :expr 'val]
        :field           ['name]                           ; 无子节点
        :protocol-impl   [:symbol 'proto-name :protocol-methods 'methods]
        :record          ['name :record-fields 'fields :protocol-impls 'protocols]
        :method          ['name :params 'params :expr 'body 'docstring] ; body 可为 nil
        :protocol        ['name :protocol-methods 'methods]
        :param           ['name]                           ; 属性
        :member-access   [:expr 'target 'accessor :exprs 'args]
        :ns              ['name 'requires 'docstring])