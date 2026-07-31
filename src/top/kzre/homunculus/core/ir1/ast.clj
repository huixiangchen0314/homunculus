(ns top.kzre.homunculus.core.ir1.ast
        (:require [top.kzre.homunculus.core.ast :refer [defast]]))

(defast IR1
        {:expr [:literal / :symbol / :call / :if / :do / :let / :fn
                :def / :loop / :recur / :quote / :var
                :throw / :try / :catch / :set!
                :vector / :map
                :record / :protocol / :member-access / :ns]
         :exprs           [:expr {:many true}]
         :binding         [:var :expr]
         :bindings        [:binding {:many true}]
         :catches         [:catch {:many true}]
         :record-fields   [:record-field {:many true}]
         :protocol-methods [:protocol-method {:many true}]
         :protocol-impls  [:protocol-impl {:many true}]
         :params          [:param {:many true}]}

        :literal   ['val]
        :symbol    ['name]
        :keyword   ['name]
        :vector    [:exprs 'items]
        :map       [:bindings 'pairs]
        :call      ['op :exprs 'args]
        :if        [:expr 'test :expr 'then :expr 'else]
        :do        [:exprs 'exprs]
        :let       [:bindings 'bindings :expr 'body]
        :fn        ['name :params 'params :expr 'body]
        :def       ['name 'doc 'attr :expr 'val]
        :loop      [:bindings 'bindings :expr 'body]
        :recur     [:exprs 'exprs]
        :quote     [:expr 'expr]
        :var       [:symbol 'var-sym]
        :throw     [:expr 'expr]
        :set!      [:var 'var :expr 'val]
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
        :ns        ['name 'docstring 'attr-map 'references])