(ns top.kzre.homunculus.core.cfg.analysis.dominator
  "支配分析。

   ── 提供 ──

   compute-dominators     —— 迭代求 immediate-dominator（Cooper-Harvey-Kennedy）
   dominance-frontier     —— 支配边界（phi 插入位置）
   dom-tree-children      —— 支配树子节点索引（重命名 DFS 用）

   ── 算法选择 ──

   Cooper-Harvey-Kennedy（迭代法）而非 Lengauer-Tarjan（半支配者法）：

   ▸ CHK 代码量小，易验证
   ▸ shader 函数通常 < 100 块——CHK 的 O(n²) 最坏情况可接受
   ▸ CHK 直接产出 immediate-dominator，不需要从半支配者反推
   ▸ 用 postorder-index 快速求交点——实际远快于 O(n²)

   ── 不可达块 ──

   从 entry 不可达的块不参与 immediate-dominator / dominance-
   frontier 计算——它们不在 postorder 的结果里。"
  (:require
    [top.kzre.homunculus.core.cfg.model :as cfg]))

;; ═══════════════════════════════════════════
;; DFS / Postorder
;; ═══════════════════════════════════════════

(defn- visit-postorder
  "DFS 后序访问。返回 [visited postorder]。"
  [cfg block-id visited postorder]
  (if (contains? visited block-id)
    [visited postorder]
    (let [[visited' postorder']
          (reduce (fn [[visited-acc postorder-acc] successor-id]
                    (visit-postorder cfg successor-id
                                     visited-acc postorder-acc))
                  [(conj visited block-id) postorder]
                  (cfg/cfg-successors-of cfg block-id))]
      [visited' (conj postorder' block-id)])))

(defn- dfs-postorder
  "从 entry 做 DFS，返回 {reverse-postorder postorder postorder-index}。

   postorder         —— 子节点先访问，entry 在末尾
   reverse-postorder —— entry 在首位
   postorder-index   —— {block-id → postorder 索引}（用于求交点快速收敛）"
  [cfg]
  (let [[_ postorder] (visit-postorder cfg (:entry cfg) #{} [])
        reverse-postorder (vec (reverse postorder))
        postorder-index   (into {}
                                (map-indexed (fn [index block-id]
                                               [block-id index]))
                                postorder)]
    {:reverse-postorder reverse-postorder
     :postorder         postorder
     :postorder-index   postorder-index}))

;; ═══════════════════════════════════════════
;; 最近公共祖先（immediate-dominator 链上）
;; ═══════════════════════════════════════════

(defn- nearest-common-dominator
  "找 block1 和 block2 在 immediate-dominator 链上的最近公共祖先。

   利用 postorder-index 快速收敛——index 小的是\"更靠近叶\"的块，
   往上走（沿 immediate-dominator 链）直到两侧相遇。"
  [block1 block2 immediate-dominators postorder-index]
  (loop [block1 block1 block2 block2]
    (cond
      (= block1 block2) block1

      (< (postorder-index block1) (postorder-index block2))
      (recur (immediate-dominators block1) block2)

      :else
      (recur block1 (immediate-dominators block2)))))

;; ═══════════════════════════════════════════
;; immediate-dominator 计算
;; ═══════════════════════════════════════════

(defn compute-dominators
  "Cooper-Harvey-Kennedy 迭代求 immediate-dominator。

   返回 {block-id → immediate-dominator-id}。
   entry 的 immediate-dominator 是自身（哨兵）——用于求交点终止。
   不可达块不出现在结果里。"
  [cfg]
  (let [{:keys [postorder postorder-index]} (dfs-postorder cfg)
        entry                               (:entry cfg)
        predecessors                        (cfg/cfg-build-predecessors cfg)
        initial-immediate-dominators        (assoc (zipmap postorder (repeat nil))
                                              entry entry)]
    (loop [immediate-dominators initial-immediate-dominators
           changed?             true]
      (if-not changed?
        immediate-dominators
        (let [immediate-dominators'
              (reduce
                (fn [acc block-id]
                  (if (= block-id entry)
                    acc
                    (let [defined-predecessors
                          (filter #(some? (get acc %))
                                  (get predecessors block-id))]
                      (if (empty? defined-predecessors)
                        acc
                        (let [new-immediate-dominator
                              (reduce (fn [candidate predecessor-id]
                                        (if (nil? candidate)
                                          predecessor-id
                                          (nearest-common-dominator
                                            candidate predecessor-id
                                            acc postorder-index)))
                                      nil
                                      defined-predecessors)]
                          (if (= new-immediate-dominator (get acc block-id))
                            acc
                            (assoc acc block-id new-immediate-dominator)))))))
                immediate-dominators
                postorder)]
          (recur immediate-dominators'
                 (not= immediate-dominators immediate-dominators')))))))

;; ═══════════════════════════════════════════
;; 支配边界
;; ═══════════════════════════════════════════

(defn dominance-frontier
  "基于 immediate-dominator 计算支配边界。

   frontier[block-id] = {x | block-id 支配 predecessor(x)，
                           但 block-id 不严格支配 x}

   返回 {block-id → #{frontier-block-id ...}}。
   没有 frontier 的块不出现在结果里。"
  [cfg immediate-dominators]
  (let [predecessors (cfg/cfg-build-predecessors cfg)]
    (reduce
      (fn [frontier block-id]
        (let [block-predecessors (get predecessors block-id)]
          (if (< (count block-predecessors) 2)
            frontier
            (let [block-immediate-dominator (get immediate-dominators block-id)]
              (reduce
                (fn [frontier predecessor-id]
                  (loop [runner   predecessor-id
                         frontier frontier]
                    (if (= runner block-immediate-dominator)
                      frontier
                      (recur (get immediate-dominators runner)
                             (update frontier runner (fnil conj #{}) block-id)))))
                frontier
                block-predecessors)))))
      {}
      (keys immediate-dominators))))

;; ═══════════════════════════════════════════
;; 支配树
;; ═══════════════════════════════════════════

(defn dom-tree-children
  "从 immediate-dominator 构建支配树的子节点索引。

   返回 {block-id → [child-id ...]}。
   entry 是根（immediate-dominator 指向自身，不是任何块的子节点）。"
  [immediate-dominators]
  (reduce
    (fn [tree [block-id parent-id]]
      (if (= block-id parent-id)
        tree
        (update tree parent-id (fnil conj []) block-id)))
    {}
    immediate-dominators))

;; ═══════════════════════════════════════════
;; 通用入口
;; ═══════════════════════════════════════════

(defn analyze
  "一次计算 immediate-dominator / dominance-frontier / dom-tree，
   返回 {immediate-dominators dominance-frontier dom-tree
         reverse-postorder postorder postorder-index}。"
  [cfg]
  (let [{:keys [reverse-postorder postorder postorder-index]} (dfs-postorder cfg)
        immediate-dominators                                 (compute-dominators cfg)
        frontier                                             (dominance-frontier cfg immediate-dominators)
        dom-tree                                             (dom-tree-children immediate-dominators)]
    {:immediate-dominators immediate-dominators
     :dominance-frontier   frontier
     :dom-tree             dom-tree
     :reverse-postorder    reverse-postorder
     :postorder            postorder
     :postorder-index      postorder-index}))


(defn dominates?
  "dominator-id 是否支配 block-id。

   沿 immediate-dominator 链从 block-id 上溯，遇到 dominator-id
   即支配。自反——块支配自身。

   entry 的 immediate-dominator 是自身——上溯到 entry 时若
   dominator-id 不是 entry，返回 false。
   不可达块（不在 idom 里）——返回 false。"
  [idom dominator-id block-id]
  (loop [current block-id]
    (cond
      (= current dominator-id) true
      (nil? current)           false
      :else
      (let [next-id (get idom current)]
        (if (or (nil? next-id) (= next-id current))
          false
          (recur next-id))))))
