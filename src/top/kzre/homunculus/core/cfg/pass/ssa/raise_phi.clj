(ns top.kzre.homunculus.core.cfg.pass.ssa.raise-phi
  "phi 提升——逐个 phi 独立分析。

   ── 结构 ──

     head:   branch c → then, else
     then:   ifval8779$$1 = texture12
     else:   ifval8779$$2 = vec4(0)
     after:  ifval8778 = ifval8779       ← 逃逸

   "
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.analysis.dominator :as dom]
    [top.kzre.homunculus.core.cfg.model :as cfg]
    [top.kzre.homunculus.core.cfg.phi :as phi])
  (:import (top.kzre.homunculus.core.cfg.model CFG)
           (top.kzre.homunculus.core.cfg.phi Phi)))

;; ═══════════════════════════════════════════
;; 遍历辅助
;; ═══════════════════════════════════════════

(defn- all-stmts
  [ssa-cfg]
  (mapcat (fn [[block-id block]]
            (map (fn [stmt]
                   [block-id stmt])
                 (:stmts block)))
          (:blocks ssa-cfg)))

(defn- escape-targets*
  [^CFG cfg var-name adapter]
  (reduce (fn [acc [block-id stmt]]
            (if (and (adapt/simple-move? adapter stmt)
                     (contains? (adapt/stmt-uses adapter stmt) var-name))
              (let [defs (adapt/stmt-defs adapter stmt)]
                (if (= 1 (count defs))
                  (conj acc
                        {:block-id block-id      ; ← 加回来
                         :target   (first defs)
                         :stmt     stmt})
                  acc))
              acc))
          #{}
          (all-stmts cfg)))

(defn find-escape
  [^CFG cfg var-name phi-block-id adapter]
  (let [targets (escape-targets* cfg var-name adapter)]
    (when (= 1 (count targets))
      (let [candidate     (first targets)
            decl-block-id (adapt/declare-block-id
                            adapter cfg
                            (:target candidate))
            idom          (dom/compute-dominators cfg)]
        (when (and decl-block-id
                   (dom/dominates? idom decl-block-id phi-block-id))
          candidate)))))

(defn phi-raisable-escape
  "phi-var 的 escape 只有一处，且在 check-block-ids 内 phi-var 的
   全部 use 就是这条 escape 语句——可以提升。

   check-block-ids = phi 块 + 所有后继可达块。phi-var 只支配这些
   块——use 只可能在这里。任何 term use、额外 stmt use 都让条件
   失败——保守但正确。"
  [^Phi phi ^CFG cfg block-id adapter]
  (let [phi-var-name  (phi/phi-var-name phi)]
    (when-let [{:keys [stmt target] :as escape}
               (find-escape cfg phi-var-name block-id adapter)]
      (let [check-block-ids (into #{block-id}
                                  (cfg/all-successors cfg block-id))
            stmt-uses       (reduce (fn [acc [bid s]]
                                      (if (and (contains? check-block-ids bid)
                                               (contains? (adapt/stmt-uses adapter s)
                                                          phi-var-name))
                                        (conj acc {:kind :stmt :block-id bid :stmt s})
                                        acc))
                                    []
                                    (all-stmts cfg))
            term-uses       (reduce (fn [acc [bid block]]
                                      (if (and (contains? check-block-ids bid)
                                               (contains? (adapt/term-uses adapter (:term block))
                                                          phi-var-name))
                                        (conj acc {:kind :term :block-id bid :term (:term block)})
                                        acc))
                                    []
                                    (:blocks cfg))
            all-uses        (into stmt-uses term-uses)]
        ;; phi-var 的全部 use 恰好是 escape 这一条——没有 term use，
        ;; 没有额外的 stmt use
        (when (and (= 1 (count all-uses))
                   (identical? stmt (:stmt (first all-uses))))
          {:phi      phi
           :target   target
           :escape   escape
           :block-id block-id})))))

(defn find-escape-phi
  "找到一个逃逸的 phi"
  [^CFG cfg adapter]
  (some (fn [[block-id block]]
          (some (fn [phi]
                  (phi-raisable-escape phi cfg block-id adapter))
                (:phis block)))
        (:blocks cfg)))

(defn- rename-phi
  "只改目标 phi 对象——identical? 避免改到同名 phi。"
  [cfg target-phi target]
  (update cfg :blocks
          (fn [blocks]
            (reduce-kv (fn [m block-id block]
                         (assoc m block-id
                                  (update block :phis
                                          (fn [phis]
                                            (mapv (fn [p]
                                                    (if (identical? p target-phi)
                                                      (phi/rename-phi-var-name p target)
                                                      p))
                                                  phis)))))
                       {}
                       blocks))))

(defn- apply-raise-escape
  "提升一个 escape phi——两件事：

   1. 删 escape 语句——phi-var 的唯一 use
   2. phi 的 var-name 改成 target——destruct 下推时 input 的
      var-name 被替换成 target，分支块的 def 自然变成 target

   不删 phi——destruct 的 transform-phi-to-version-def 需要它
   来下推。不 push inputs——下推由 destruct 负责，重复会出错。"
  [cfg {:keys [phi target escape]} adapter]
  (let [escape-stmt  (:stmt escape)
        escape-block (:block-id escape)]
    (-> cfg
        ;; 删除逃逸语句
        (update-in [:blocks escape-block :stmts]
                   (fn [stmts]
                     (vec (remove #(identical? % escape-stmt) stmts))))
        ;; phi 重命名
        (rename-phi phi target))))

(defn raise-phi
  [cfg adapter ]
  (loop [cfg       cfg
         iteration 0]
    (when (> iteration 10000)
      (throw (ex-info "raise-phi did not converge" {:iterations iteration})))
    (if-let [candidate (find-escape-phi cfg adapter)]
      (recur (apply-raise-escape cfg candidate adapter) (inc iteration))
      cfg)))