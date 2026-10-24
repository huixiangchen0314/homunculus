(ns top.kzre.homunculus.core.cfg.irstmt.varversion
  "IRStmt 层的变量版本化降级。

   ── lower（降级）──

   把所有 `x$$N` 合并回 `x`——SSA 构造后的清理，用于 emit。
   同时删除自赋值 `x = x`（phi 消除方法 2 的 move）。

   SSA 构造在 CFG 层版本化（x → x$$N）——phi-var 和 var-decl、
   use 的版本号不一致。GLSL 里 `x$$1` / `x$$2` 是不同变量——
   `x$$1` 的声明对 `x$$2` 无效。lower 后全部合并到 x——声明
   一次，use 全部匹配。

   phi 消除方法 2 产生的 move 在 lower 后变成 x = x——删除。

   ── 与 CFG SSA 的关系 ──

   lower 是 CFG 层 SSA 构造（construct）的逆操作——construct
   版本化，lower 全部 strip 回来。是 emit 前的必需步骤。"
  (:require
   [clojure.string :as str]
   [top.kzre.homunculus.core.cfg.pass.ssa.construct :as construct]
   [top.kzre.homunculus.core.irstmt.ast :as ast]))

;; ═══════════════════════════════════════════
;; 公共辅助
;; ═══════════════════════════════════════════

(defn- strip-version
  "x$$N → x。无 $$ 时原样返回。"
  [sym]
  (let [s (name sym)
        i (str/last-index-of s (construct/version-separator))]
    (if i (symbol (subs s 0 i)) sym)))

(defn- self-assign?
  "x = x 形式的自赋值——phi 消除方法 2 的 move。"
  [stmt]
  (and (= :assign (ast/kind stmt))
       (= :variable (ast/kind (:var stmt)))
       (= :variable (ast/kind (:val stmt)))
       (= (:name (:var stmt)) (:name (:val stmt)))))

;; ═══════════════════════════════════════════
;; lower
;; ═══════════════════════════════════════════

(defn- lower-node
  "递归 strip 所有 x$$N 后缀。

   reduce-children 返回 [new-node env] 二元组——取 first。"
  [node]
  (cond
    (nil? node) nil

    (= :variable (ast/kind node))
    (assoc node :name (strip-version (:name node)))

    (= :var-decl (ast/kind node))
    (-> node
        (assoc :name (strip-version (:name node)))
        (update :val lower-node))

    (= :assign (ast/kind node))
    (-> node
        (update :var lower-node)
        (update :val lower-node))

    :else
    (first (ast/reduce-children node
                                (fn [c _] [(lower-node c) nil])
                                nil))))

(defn- strip-self-assign
  "递归删除 block stmts 里的自赋值 x = x。

   reduce-children 返回 [new-node env] 二元组——取 first。"
  [node]
  (cond
    (nil? node) nil

    (= :block (ast/kind node))
    (-> node
        (update :stmts (fn [ss]
                         (->> ss
                              (map strip-self-assign)
                              (remove self-assign?)
                              vec)))
        (update :ret strip-self-assign))

    :else
    (first (ast/reduce-children node
                                (fn [c _] [(strip-self-assign c) nil])
                                nil))))

(defn lower
  "把 SSA 版本化名字合并回 base name，删自赋值。

   lower 是 SSA 构造的逆操作——所有 x$$N 变成 x。
   用于 emit 前的清理。

   两步：
   ▸ lower-node        —— 递归 strip 所有 $$N 后缀
   ▸ strip-self-assign —— 删除 phi 消除方法 2 产生的 x = x"
  [ast]
  (-> ast lower-node strip-self-assign))