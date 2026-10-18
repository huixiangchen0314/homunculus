(ns top.kzre.homunculus.backend.shader.pass.split-entries
  "按入口点分流 shader AST。

   一个 shader 源可能包含多个入口点（如 VS + FS 共存）。每个入口点
   需要编译为独立的 GLSL 程序。

   ── 分流规则 ──────────────────────────────────────────
   ▸ 每个入口点一组
   ▸ 组内包含：该入口点 + 全部非入口节点（声明、函数、结构体等）
   ▸ 不做函数可达性过滤——GLSL 编译器会剔除未使用的函数
   ▸ 组内不含其他入口点——避免生成多个 main"
  (:require
    [top.kzre.homunculus.backend.shader.ast :as ast]))

;; ═══════════════════════════════════════════
;; 节点判定
;; ═══════════════════════════════════════════

(defn- entry-point? [node]
  (= :entry-point (ast/kind node)))

;; ═══════════════════════════════════════════
;; 分流
;; ═══════════════════════════════════════════
(defn split
  "按入口点分流 asts。返回 [{:entry ... :asts [...]} ...]。

   每个组内的 asts 顺序：全部非入口节点（保持原顺序）+ 当前入口点。
   入口点排在最后——符合 GLSL 惯例（main 在文件末尾）。

   如果 asts 里没有入口点——返回空向量。"
  [asts]
  (let [entries (filter entry-point? asts)
        others  (remove entry-point? asts)]
    (mapv (fn [entry]
            {:entry entry
             :asts  (conj (vec others) entry)})
          entries)))