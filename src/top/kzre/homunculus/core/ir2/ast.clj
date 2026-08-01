(ns top.kzre.homunculus.core.ir2.ast
        "IR2 语言无关的中间表示 (IR) 定义。
         IR2 与 IR1 保持结构同构：IR1 中的 Pair, Param, Binding, Catch, RecordField,
         ProtocolImpl, ProtocolMethod 等显式节点在 IR2 中均有完全对应的节点。
         降级 (lowering) 过程仅进行 1:1 映射，不破坏或展开任何结构。
         所有语义分析与变换（闭包捕获、协议方法提升、类型推导等）均作为独立 Pass 在 IR2 上执行。

         使用 defast 宏生成：
         - IR2 协议（含 kind, children, map-children, node-meta）
         - 全部 AST 节点记录
         - 通用树遍历函数 postwalk / prewalk / postwalk-env / prewalk-env

         节点字段说明：
         - 属性字段以 'name 形式表示。
         - 子节点字段以 :type 或 :type 'field 形式表示。
         - 多子节点字段（如参数列表、表达式列表）在类型声明中带有 {:many true}。
         - 所有节点记录自动包含 meta 字段，可通过 node-meta 获取。
         - 记录名由节点 kind 关键字首字母大写得到（无 Node 后缀）。
         - attrs 字段统一放在字段列表最后，用于存放编译器属性，供优化 pass 使用。"
        (:require [top.kzre.homunculus.core.ast :refer [defast]]))

;; ── 声明自动生成的符号，辅助 IDE 提示 ──
(declare prewalk prewalk-env postwalk postwalk-env)
(declare IR2 kind children map-children node-meta)
(declare ->Literal ->Variable ->Lambda ->Call ->If ->Block ->Let ->Loop ->Recur
        ->Define ->Vector ->Map ->Try ->Catch ->Throw ->Assign ->While ->Convert
        ->Ns ->Record ->RecordField ->Protocol ->MemberAccess
        ->NewArray ->Aget ->Aset ->Alength ->Binding
        ->Param ->Pair ->ProtocolImpl ->ProtocolMethod)

(defast IR2
        ;; ═══════════════════════════════════════════════
        ;; 类型声明
        ;; ═══════════════════════════════════════════════
        {:expr [:literal / :variable / :call / :if / :block / :let / :loop
                :recur / :define / :lambda / :vector / :map / :try / :catch
                :throw / :assign / :while / :convert / :ns / :record
                :protocol / :member-access /
                :new-array / :aget / :aset / :alength]
         :exprs        [:expr {:many true}]
         :bindings     [:binding {:many true}]
         :catches      [:catch {:many true}]
         :params       [:param {:many true}]
         :pairs        [:pair {:many true}]
         :record-fields [:record-field {:many true}]
         :protocol-methods [:protocol-method {:many true}]
         :protocol-impls   [:protocol-impl {:many true}]}

        ;; ═══════════════════════════════════════════════
        ;; 节点定义（attrs 统一在最后）
        ;; ═══════════════════════════════════════════════

        ;; ── 基础 ──
        :literal   ['val 'attrs]
        :variable  ['name 'attrs]

        ;; ── 参数（显式节点，对应 IR1 的 Param） ──
        :param     [:variable 'name 'attrs]

        ;; ── 键值对（显式节点，对应 IR1 的 Pair） ──
        :pair      [:expr 'key :expr 'val 'attrs]

        ;; ── Lambda ──
        :lambda    ['fn-name               ;; 属性（闭包捕获，由分析 Pass 填充）
                    :params 'params                 ;; 多 Param
                    :expr 'body                     ;; 单表达式
                    'attrs]

        ;; ── 调用 ──
        :call      [:expr 'fn :exprs 'args 'attrs]   ;; fn 可为 nil

        ;; ── 条件 ──
        :if        [:expr 'test :expr 'then :expr 'else 'attrs]

        ;; ── 块 ──
        :block     [:exprs 'exprs 'attrs]

        ;; ── 绑定 ──
        :let       [:bindings 'bindings :expr 'body 'attrs]
        :loop      [:bindings 'bindings :expr 'body 'attrs]

        ;; ── 递归 ──
        :recur     [:exprs 'args 'attrs]

        ;; ── 定义 ──
        :define    ['name 'doc :expr 'val 'attrs]     ;; val 可能为 nil

        ;; ── 向量 / Map ──
        :vector    [:exprs 'items 'attrs]
        :map       [:pairs 'pairs 'attrs]

        ;; ── 异常 ──
        :try       [:expr 'body :catches 'catches :expr 'finally 'attrs]
        :catch     ['class 'sym :exprs 'body 'attrs]

        ;; ── 抛出 / 赋值 ──
        :throw     [:expr 'expr 'attrs]
        :assign    [:variable 'var :expr 'val 'attrs]

        ;; ── 循环 / 类型转换 ──
        :while     [:expr 'test :expr 'body 'attrs]
        :convert   ['src-ty 'dst-ty 'cost :expr 'expr 'attrs]

        ;; ── 命名空间 ──
        :ns        ['name 'doc 'requires 'attrs]

        ;; ── 记录（保留字段和协议实现） ──
        :record    ['name
                    :record-fields 'fields
                    :protocol-impls 'protocols
                    'attrs]
        :record-field ['name :expr 'init 'attrs]       ;; init 可选

        ;; ── 协议（仅保留签名） ──
        :protocol  ['name :protocol-methods 'funcs 'attrs]

        ;; ── 协议实现与方法（显式节点，后续 Pass 可提升） ──
        :protocol-impl   [:symbol 'protocol-sym
                          :protocol-methods 'methods
                          'attrs]
        :protocol-method ['name :params 'params :expr 'body 'attrs]

        ;; ── 成员访问 ──
        :member-access [:expr 'target 'accessor :exprs 'args 'attrs]

        ;; ── 数组操作 ──
        :new-array ['size 'attrs]
        :aget      [:expr 'target :expr 'idx 'attrs]
        :aset      [:expr 'target :expr 'idx :expr 'val 'attrs]
        :alength   [:expr 'target 'attrs]

        ;; ── 绑定对（let/loop 使用） ──
        :binding   [:variable 'var :expr 'val 'attrs])