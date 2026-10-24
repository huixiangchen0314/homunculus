(ns top.kzre.homunculus.core.cfg.analysis.dominator
  "支配分析。

   ── 提供 ────────────────────────────────────────────

   compute-dominators   —— 迭代求 idom（Cooper-Harvey-Kennedy）
   dominance-frontier   —— 支配边界（phi 插入位置）
   dom-tree-children    —— 支配树子节点索引（重命名 DFS 用）

   ── 算法选择 ────────────────────────────────────────

   Cooper-Harvey-Kennedy（迭代法）而非 Lengauer-Tarjan（半支配者法）：

   ▸ CHK 代码量小（~50 行 vs ~200 行），易验证
   ▸ shader 函数通常 < 100 块——CHK 的 O(n²) 最坏情况可接受
   ▸ CHK 直接产出 idom，不需要从半支配者反推
   ▸ 用 dfs-order 快速求交点——实际远快于 O(n²)

   ── 不可达块 ────────────────────────────────────────

   从 entry 不可达的块不参与 idom / DF 计算——它们不在 dfs-postorder
   的结果里。构造 CFG 时 statementize 的展开通常不产生不可达块，
   但 throw 之后的语句可能——build 侧已经丢弃，这里不再检查。

   ── 算法复杂度 ──────────────────────────────────────

   dfs-postorder         O(V + E)
   compute-dominators    O(V²) 最坏，实测接近 O(V)
   dominance-frontier    O(V · E)
   dom-tree-children     O(V)"
  (:require
    [top.kzre.homunculus.core.cfg.model :as cfg]))

;; ═══════════════════════════════════════════
;; DFS / Postorder
;; ═══════════════════════════════════════════

(defn- dfs-postorder
  "从 entry 做 DFS，返回 {rpo post dfs-order}。

   post       —— postorder 序列（子节点先访问，entry 在末尾）
   rpo        —— reverse postorder（entry 在首位）
   dfs-order  —— {block-id → postorder 索引}（用于 intersect 快速收敛）

   不可达块不出现在任何结果里。"
  [cfg]
  (let [visited (atom #{})
        post    (atom [])]
    (letfn [(visit [id]
              (when-not (contains? @visited id)
                (swap! visited conj id)
                (doseq [succ (cfg/cfg-successors-of cfg id)]
                  (visit succ))
                (swap! post conj id)))]
      (visit (:entry cfg)))
    (let [post      @post
          rpo       (vec (reverse post))
          dfs-order (into {}
                          (map-indexed (fn [i id] [id i]))
                          post)]
      {:rpo       rpo
       :post      post
       :dfs-order dfs-order})))

;; ═══════════════════════════════════════════
;; idom 计算（Cooper-Harvey-Kennedy）
;; ═══════════════════════════════════════════

(defn- intersect
  "找 b1 和 b2 在 idom 链上的最近公共祖先。

   利用 dfs-order 快速收敛——order 小的是\"更靠近叶\"的块，
   往上走（沿 idom 链）直到两侧相遇。

   postorder 编号：叶节点小，entry 最大。"
  [b1 b2 idom dfs-order]
  (loop [b1 b1 b2 b2]
    (cond
      (= b1 b2) b1

      ;; b1 更靠近叶——往上走
      (< (dfs-order b1) (dfs-order b2))
      (recur (idom b1) b2)

      ;; b2 更靠近叶——往上走
      :else
      (recur b1 (idom b2)))))

(defn compute-dominators
  "Cooper-Harvey-Kennedy 迭代求 idom。

   返回 {block-id → idom-id}。
   entry 的 idom 是自身（哨兵）——用于 intersect 终止。
   不可达块不出现在结果里。

   算法：
   1. 从 entry 做 DFS 得 postorder
   2. 初始 idom[entry] = entry，其他 nil
   3. 迭代遍历 postorder（除 entry）：
        找所有前驱中 idom 已确定的，求它们的 idom 交点
        更新 idom[b] 为该交点
      直到不动点

   \"前驱中 idom 已确定的\"——可达块的前驱也是可达的，
   但迭代过程是逐层收敛的（离 entry 近的先确定）。"
  [cfg]
  (let [{:keys [post dfs-order]} (dfs-postorder cfg)
        entry                     (:entry cfg)
        preds                     (cfg/cfg-build-preds cfg)
        ;; 初始 idom：entry 自身，其他 nil
        init-idom                 (assoc (zipmap post (repeat nil))
                                    entry entry)]
    (loop [idom     init-idom
           changed? true]
      (if-not changed?
        idom
        (let [idom'
              (reduce
                (fn [acc b]
                  (if (= b entry)
                    acc
                    (let [defined-preds
                          (filter #(some? (get acc %))
                                  (get preds b))]
                      (if (empty? defined-preds)
                        acc
                        (let [new-idom
                              (reduce (fn [x p]
                                        (if (nil? x)
                                          p
                                          (intersect x p acc dfs-order)))
                                      nil
                                      defined-preds)]
                          (if (= new-idom (get acc b))
                            acc
                            (assoc acc b new-idom)))))))
                idom
                post)]
          (recur idom' (not= idom idom')))))))

;; ═══════════════════════════════════════════
;; 支配边界
;; ═══════════════════════════════════════════

(defn dominance-frontier
  "基于 idom 计算支配边界。

   DF[b] = {x | b 支配 pred(x)，但 b 不严格支配 x}

   算法：
   对每个有两个以上前驱的块 b：
     for p in preds(b):
       runner = p
       while runner != idom[b]:
         DF[runner] += b
         runner = idom[runner]

   直观理解：b 有多个前驱，是路径汇合点——b 处可能需要 phi。
   每个前驱 p 沿 idom 链上溯到 idom[b]（不含），沿途每个块
   \"对 b 有贡献但不支配 b\"，b 加入其 DF。

   idom[b] 本身支配 b，b 不加入 idom[b] 的 DF——循环停在
   idom[b] 之前。

   返回 {block-id → #{df-block-id ...}}。
   没有 DF 的块不出现在结果里。"
  [cfg idom]
  (let [preds (cfg/cfg-build-preds cfg)]
    (reduce
      (fn [df b]
        (let [b-preds (get preds b)]
          (if (< (count b-preds) 2)
            df
            (let [b-idom (get idom b)]
              (reduce
                (fn [df p]
                  (loop [runner p
                         df     df]
                    (if (= runner b-idom)
                      df
                      (recur (get idom runner)
                             (update df runner (fnil conj #{}) b)))))
                df
                b-preds)))))
      {}
      (keys idom))))

;; ═══════════════════════════════════════════
;; 支配树
;; ═══════════════════════════════════════════

(defn dom-tree-children
  "从 idom 构建支配树的子节点索引。

   返回 {block-id → [child-id ...]}。
   entry 是根（idom 指向自身，不是任何块的子节点）。
   其他块挂在各自的 idom 下。

   重命名阶段的 DFS 遍历用此结构——从 entry 开始，
   按支配树递归，维护每个变量的版本栈。"
  [idom]
  (reduce
    (fn [tree [b i]]
      (if (= b i)
        tree
        (update tree i (fnil conj []) b)))
    {}
    idom))

;; ═══════════════════════════════════════════
;; 通用入口
;; ═══════════════════════════════════════════

(defn analyze
  "一次计算 idom / DF / dom-tree，返回 {idom df dom-tree rpo post dfs-order}。

   方便 SSA 构造一次调用拿全所有支配信息——
   避免分别调用时的重复 DFS 和 preds 构建。"
  [cfg]
  (let [{:keys [rpo post dfs-order]} (dfs-postorder cfg)
        idom                          (compute-dominators cfg)
        df                            (dominance-frontier cfg idom)
        dom-tree                      (dom-tree-children idom)]
    {:idom      idom
     :df        df
     :dom-tree  dom-tree
     :rpo       rpo
     :post      post
     :dfs-order dfs-order}))