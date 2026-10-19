(ns top.kzre.homunculus.backend.shader.pass.liveness
  "顶层符号的活跃分析。

   从入口点出发，标记所有可达的顶层声明——函数、struct、全局变量。

   ── 可达关系 ──────────────────────────────────────────
   入口点   → 引用的函数、变量、类型
   函数     → 引用的函数、变量、类型
   struct   → 成员类型引用的其他 struct
   全局变量 → 类型引用的 struct

   ── 标记 ──────────────────────────────────────────────
   可达的顶层节点在 [:attrs :living?] 标 true。
   不修改节点结构——emit / lower 层按需读取标记。

   ── 保守性 ────────────────────────────────────────────
   局部变量名与顶层符号名可能碰撞——误标记为活跃不影响正确性。
   仅可能保留未使用的声明——后续死代码消除层再处理。"
  (:require
    [top.kzre.homunculus.backend.shader.ast :as ast]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

;; ═══════════════════════════════════════════
;; 引用抽取
;; ═══════════════════════════════════════════

(defn- type-ref-names
  "从 IR 类型中提取引用的类型名集合。递归 TVec 的元素。"
  [ir-type]
  (cond
    (nil? ir-type)          #{}
    (ty/type-sym ir-type)   #{(ty/type-sym ir-type)}
    (ty/vec-type? ir-type)  (type-ref-names (ty/vec-element-type ir-type))
    :else                   #{}))

(defn- node-refs
  "单个节点的直接引用集合——函数名、变量名、类型名。

   ▸ :call 的 :fn —— 函数引用
   ▸ :variable 的 :name —— 变量引用
   ▸ :type / :return-type —— 类型引用（含嵌套 TVec 的元素类型）"
  [node]
  (let [kind (ast/kind node)
        refs (cond-> #{}
                     (and (= kind :call) (some? (:fn node)))
                     (conj (:fn node))

                     (and (= kind :variable) (some? (:name node)))
                     (conj (:name node)))]
    (-> refs
        (into (type-ref-names (:type node)))
        (into (type-ref-names (:return-type node))))))

(defn- walk-refs
  "深度遍历节点树，收集所有引用。"
  [node]
  (let [acc (volatile! #{})]
    (letfn [(walk [n]
              (vswap! acc into (node-refs n))
              (ast/reduce-children n
                                   (fn [child _] (walk child) [child nil])
                                   nil))]
      (walk node))
    @acc))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn analyze
  "顶层符号的活跃分析，标记在 [:attrs :living?] true。

   asts —— 顶层 ShaderAST 列表。入口点由 :entry-point 节点类型
           天然标识——不需要额外传入入口名集合。

   返回新的 asts——活跃节点的 [:attrs :living?] 为 true。
   未标记的节点保持原状——不主动标 false。"
  [asts]
  (let [by-name (into {}
                      (keep (fn [node]
                              (when-let [n (:name node)]
                                [n node])))
                      asts)
        roots   (filter (fn [node]
                          (= :entry-point (ast/kind node)))
                        asts)
        living  (loop [queue (vec (mapcat walk-refs roots))
                       seen  (set (keep :name roots))]
                  (if (empty? queue)
                    seen
                    (let [n     (peek queue)
                          queue (pop queue)]
                      (if (contains? seen n)
                        (recur queue seen)
                        (let [node     (get by-name n)
                              new-refs (if node (walk-refs node) #{})]
                          (recur (into queue new-refs)
                                 (conj seen n)))))))]
    (mapv (fn [node]
            (if (and (some? (:name node))
                     (contains? living (:name node)))
              (assoc-in node [:attrs :living?] true)
              node))
          asts)))

;; ═══════════════════════════════════════════
;; 清除
;; ═══════════════════════════════════════════

(def ^:private eliminable-kinds
  "可被活跃分析移除的节点类型——纯声明，无引用则无副作用。

   不包含：
     :entry-point  —— 入口必须保留
     :uniform      —— 资源声明；无引用时 GLSL 编译器会优化掉，
                      但源码保留有助于调试
     :texture-decl / :sampler-decl —— 配对分析需要
     :cbuffer-decl —— 成员引用不传播到 cbuffer 节点本身
     :import       —— 预处理指令，不可分析
   以上类型始终保留。"
  #{:function :struct :static-var})

(defn elim
  "根据活跃分析移除未标记的节点。

   只移除 eliminable-kinds 且未标 living? 的节点。
   其余节点原样保留。

   :keeps 强制保留的顶级符号。"
  [asts & {:keys [keeps]
           :or {keeps #{}}}]
  (into []
        (remove (fn [node]
                  (and (contains? eliminable-kinds (ast/kind node))
                       (not (contains? keeps (:name node)))
                       (not (get-in node [:attrs :living?])))))
        asts))