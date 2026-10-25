(ns top.kzre.homunculus.core.cfg.pass.reachable
  "空块消除——删除没有语句的跳转块。

   ── 什么是空块 ──

   ▸ stmts 为空
   ▸ term 是 :jump（无条件跳转）
   ▸ 不是 loop-head（while head 的 stmts 可能为空，但它是循环头）
   ▸ 不是 entry（入口块必须有）

   空块是 build 时 if-else 的 after 块在后续没有语句时产生的
   ——then/else 都跳转到 after，after 跳到 parent-target。
   after 没有 stmts——空块。

   ── 删除方式 ──

   把指向空块的跳转重定向到空块的后继，然后删除空块。
   迭代直到没有空块——删除一个空块可能产生新的空块（连续
   空块）。

   ── 为什么事后清理 ──

   build 时是 reduce，处理 if 时看不到后续 stmt——不知道 after
   是否会是空块。事后清理更简单、更标准（LLVM 的 unreachable
   block elimination）。"
  (:require
    [top.kzre.homunculus.core.cfg.model :as cfg]
    [top.kzre.homunculus.core.cfg.protocol :as proto])
  (:import (top.kzre.homunculus.core.cfg.model CFG)))

;; ═══════════════════════════════════════════
;; 空块判定
;; ═══════════════════════════════════════════

(defn- empty-jump-block?
  "块是空块——没有 stmts，term 是 Jump，不是 loop-head，
   不是 entry。"
  [^CFG cfg block-id block]
  (and (not= block-id (:entry cfg))
       (empty? (:stmts block))
       (= :jump (proto/term-kind (:term block)))
       (not (cfg/cfg-loop-head? cfg block-id))))

;; ═══════════════════════════════════════════
;; 删除空块
;; ═══════════════════════════════════════════

(defn- redirect-predecessors
  "把所有指向 old-block-id 的后继重定向到 new-block-id。"
  [^CFG cfg old-block-id new-block-id]
  (reduce-kv
    (fn [c block-id block]
      (if (some #{old-block-id} (proto/bb-successors block))
        (cfg/cfg-replace-successor c block-id old-block-id new-block-id)
        c))
    cfg
    (:blocks cfg)))

(defn- remove-empty-block
  "删除空块——前驱重定向到空块的后继。"
  [^CFG cfg block-id]
  (let [block     (cfg/cfg-block cfg block-id)
        successor (:target (:term block))]
    (-> cfg
        (redirect-predecessors block-id successor)
        (cfg/cfg-remove-block block-id))))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn elim
  "删除所有空跳转块——迭代直到没有空块。

   返回新的 CFG。"
  [^CFG cfg]
  (loop [c cfg]
    (let [empty-block (some (fn [[block-id block]]
                              (when (empty-jump-block? c block-id block)
                                block-id))
                            (:blocks c))]
      (if (nil? empty-block)
        c
        (recur (remove-empty-block c empty-block))))))