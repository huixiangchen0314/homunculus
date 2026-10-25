(ns top.kzre.homunculus.core.cfg.bake
  "烘焙 CFG 回 IRStmt AST。

   ── 结构与参数 ──

   BakeCtx 打包不可变上下文（cfg / adapter / loop-head-id / stop-at-id）。
   bake-block* 及所有分支函数接收 [ctx ... visited]，visited 是纯集合
   ——显式传递，返回值带 visited'。无 atom。

   每个 :branch / :jump / :throw / :return 分支提取成独立函数：
   ▸ bake-return   [block visited]
   ▸ bake-throw    [ctx block visited]
   ▸ bake-jump     [ctx block visited target]
   ▸ bake-branch   [ctx block visited]
   ▸ bake-branch-join [ctx block visited join-id]
   ▸ bake-while    [ctx block visited]

   所有函数返回 [stmts ret stopped? visited'] 四元组。

   ── while head 直接查 :loop-heads ──

   build 时 handle-while 记录 head-id 到 CFG 的 :loop-heads。
   bake 的 while head 判定直接查集合——O(1)，精确，不做回边检测。

   ── 终止块允许多路径到达 ──

   非终止块做 visited 排他检查。终止块（:return / :throw）不做
   ——多路径 return 到同一块是合法 CFG 结构。"
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.model :as cfg]
    [top.kzre.homunculus.core.cfg.protocol :as proto])
  (:import (top.kzre.homunculus.core.cfg.model CFG)))

(defrecord BakeCtx [cfg adapter loop-head-id stop-at-id])

(defn- make-ctx
  [^CFG cfg adapter]
  (->BakeCtx cfg adapter nil nil))

;; ═══════════════════════════════════════════
;; 分支函数——每个返回 [stmts ret stopped? visited']
;; ═══════════════════════════════════════════

(declare bake-block*)

(defn- bake-return
  [block visited]
  [(:stmts block) (:value (:term block)) false visited])

(defn- bake-throw
  [{:keys [adapter]} block visited]
  [(conj (vec (:stmts block))
         (adapt/make-throw adapter (:value (:term block))))
   nil
   false
   visited])

(defn- bake-jump
  [{:keys [loop-head-id stop-at-id] :as ctx} block visited target]
  (let [stmts (:stmts block)]
    (cond
      ;; 到达外层停止点——返回，调用方继续
      (and (some? stop-at-id) (= target stop-at-id))
      [stmts nil true visited]

      ;; 到达循环头——while body 结束
      (and (some? loop-head-id) (= target loop-head-id))
      [stmts nil false visited]

      :else
      (let [[more-stmts ret stopped visited']
            (bake-block* ctx target visited)]
        [(into (vec stmts) more-stmts) ret stopped visited']))))

(defn- bake-branch-join
  "汇合 if——then/else 递归到 join 停止，merge 用外层上下文继续。"
  [{:keys [adapter loop-head-id stop-at-id] :as ctx}
   block visited join-id]
  (let [term      (:term block)
        test      (:test term)
        then-id   (:then term)
        else-id   (:else term)
        ctx-inner (assoc ctx :stop-at-id join-id)
        [then-stmts _ _ visited1] (bake-block* ctx-inner then-id visited)
        [else-stmts _ _ visited2] (bake-block* ctx-inner else-id visited1)
        then-block (adapt/make-block adapter then-stmts nil)
        else-block (adapt/make-block adapter else-stmts nil)
        if-node    (adapt/make-if adapter test then-block else-block)
        [merge-stmts merge-ret merge-stopped visited3]
        (cond
          ;; join 就是外层 stop-at——停止，调用方继续
          (and (some? stop-at-id) (= join-id stop-at-id))
          [[] nil true visited2]

          ;; join 就是循环头——while body 结束
          (and (some? loop-head-id) (= join-id loop-head-id))
          [[] nil false visited2]

          :else
          ;; merge 用外层 ctx——loop-head-id 和 stop-at-id 都是调用方传入的
          (bake-block* ctx join-id visited2))]
    [(into (vec (:stmts block)) (into [if-node] merge-stmts))
     merge-ret
     merge-stopped
     visited3]))

(defn- bake-branch
  [{:keys [cfg] :as ctx} block visited]
  (let [branch    (:term block)
        join-id (cfg/cfg-join-block-id cfg branch)]
    (if (nil? join-id)
      ;; 各自 return——emit 层不支持 if 分支 return，直接拒绝
      (throw (ex-info "bake: if with both branches return not supported by emit"
                      {:block-id (:id block)}))
      (bake-branch-join ctx block visited join-id))))


(defn- enter-loop-body
  "body：loop-head-id = 当前 head；stop-at 清空——body 是封闭的循环体"
  [^BakeCtx ctx head-id]
  (assoc ctx :loop-head-id head-id :stop-at-id nil))

(defn- bake-while
  [{:keys [adapter] :as ctx} block visited]
  (let [head-id (:id block)
        branch (:term block)
        test    (:test branch)
        body-id (:then branch)
        exit-id (:else branch)

        [body-stmts body-ret _ visited1]
        (bake-block* (enter-loop-body ctx head-id)
                      body-id visited)]
    (when (some? body-ret)
      (throw (ex-info "bake: while body with return not supported"
                      {:block-id head-id :body-ret body-ret})))
    (let [body-block (adapt/make-block adapter body-stmts nil)
          while-node (adapt/make-while adapter test body-block )
          ;; exit：用外层 ctx——保留外层 loop-head-id 和 stop-at-id。
          ;; 外层是 while body 时，exit 后续 jump 到外层 head 能正确停止；
          ;; 外层是汇合 if 的 then 时，exit 后续到外层 join 能正确停止。
          [exit-stmts exit-ret exit-stopped visited2]
          (bake-block* ctx exit-id visited1)]
      [(into (vec (:stmts block)) (into [while-node] exit-stmts))
       exit-ret
       exit-stopped
       visited2])))

;; ═══════════════════════════════════════════
;; 主递归
;; ═══════════════════════════════════════════

(defn- bake-block*
  "bake 核心递归。返回 [stmts ret stopped? visited']。

   终止块不做 visited 排他检查——允许多路径 return。"
  [^BakeCtx ctx block-id visited]
  (let [cfg       (:cfg ctx)
        block     (cfg/cfg-block cfg block-id)
        term      (:term block)
        term-kind (proto/term-kind term)]
    (when (contains? visited block-id)
      (throw (ex-info "bake: block already visited (cycle or unsupported)"
                      {:block-id   block-id
                       :loop-head  (:loop-head-id ctx)
                       :stop-at    (:stop-at-id ctx)
                       :visited    visited})))
    (let [visited' (conj visited block-id)]
      (case term-kind
        :return (bake-return block visited')
        :throw  (bake-throw ctx block visited')
        :jump   (bake-jump ctx block visited' (:target term))
        :branch (if (contains? (:loop-heads cfg #{}) block-id)
                  ;; 循环头
                  (bake-while ctx block visited')
                  (bake-branch ctx block visited'))))))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn bake
  [^CFG cfg adapter]
  (let [ctx             (make-ctx cfg adapter)
        [stmts ret _ _] (bake-block* ctx (:entry cfg) #{})]
    (adapt/make-block adapter stmts ret)))