(ns top.kzre.homunculus.core.ir1.ast
        (:require [top.kzre.homunculus.core.ast :refer [defast]]))

(declare prewalk prewalk-env postwalk postwalk-env)
(declare IR1 kind children map-children node-meta)
(declare ->Literal ->Symbol ->Keyword ->Vector ->Map ->Call ->If
        ->Do ->Let ->Fn ->Def ->Loop ->Recur ->Quote ->Var
        ->Throw ->Set ->Try ->Catch ->Binding
        ->RecordField ->ProtocolImpl ->Record
        ->ProtocolMethod ->Protocol
        ->Param ->MemberAccess ->Ns ->Pair)

(defonce special-forms
         '#{ns fn let loop recur quote var set! try catch throw
            if do . def defrecord defprotocol})

(defast IR1
        {:expr [:literal / :symbol / :call / :if / :do / :let / :fn
                :def / :loop / :recur / :quote / :var
                :throw / :try / :catch / :set
                :vector / :map
                :record / :protocol / :member-access / :ns]
         :exprs           [:expr {:many true}]
         :binding         [:var :expr]
         :bindings        [:binding {:many true}]
         :catches         [:catch {:many true}]
         :record-fields   [:record-field {:many true}]
         :protocol-methods [:protocol-method {:many true}]
         :protocol-impls  [:protocol-impl {:many true}]
         :params          [:param {:many true}]
         :pairs  [:pair {:many true}]}

        :literal   ['val]
        :symbol    ['name]
        :keyword   ['ns 'name]
        :vector    [:exprs 'items]
        :pair      [:expr 'key :expr 'val]
        :map       [:pairs 'pairs]
        :call      ['op :exprs 'args]
        :if        [:expr 'test :expr 'then :expr 'else]
        :do        [:exprs 'exprs]
        :let       [:bindings 'bindings :expr 'body]
        :fn        ['name :params 'params :expr 'body]
        :def       ['name 'doc :expr 'val]
        :loop      [:bindings 'bindings :expr 'body]
        :recur     [:exprs 'exprs]
        :quote     [:expr 'expr]
        :var       [:symbol 'var-sym]
        :throw     [:expr 'expr]
        :set       [:var 'var :expr 'val]
        :try       [:expr 'body :catches 'catches :expr 'finally]
        :catch     ['class 'sym :expr 'body]
        :binding   [:var 'var :expr 'val]
        :record-field      ['name :expr 'init]
        :protocol-impl     [:symbol 'protocol-sym
                            :protocol-methods 'methods]
        :record            ['name
                            :record-fields 'fields
                            :protocol-impls 'protocols]
        :protocol-method   ['name :params 'params :expr 'body]
        :protocol          ['name :protocol-methods 'funcs]
        :param             ['name]
        :member-access [:expr 'target 'accessor :exprs 'args]
        :ns        ['name 'doc 'requires])


