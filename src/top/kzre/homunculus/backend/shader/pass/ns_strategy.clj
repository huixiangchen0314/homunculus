(ns top.kzre.homunculus.backend.shader.pass.ns-strategy
  "在 ShaderAST 上统一应用命名空间策略。

   ▸ IR2 层已降级——符号表查询不再需要，改名不破坏类型检查
   ▸ analyze / sematic / lower / emit 全部基于改名后的名字工作，
     规划产物天然一致
   ▸ 在 GlslAST 层做太晚——规划产物已生成，改名会破坏一致性

   策略：
   ▸ :strip  —— 去掉命名空间，只保留 name 部分
                 top.kzre.../myTexture → myTexture
   ▸ :mangle —— 全限定名整体转义
                 top.kzre.../myTexture → top_kzre_..._myTexture

   改名范围：
   ▸ 节点 :name / :fn —— 符号直接改
   ▸ 节点 :type / :return-type / :element-type —— 递归改 TCon 符号
     （TVec 元素、TFun 参数和返回也递归）
   ▸ :params / :struct-members 是 ast 的子节点声明——param /
     struct-member 节点由 reduce-children 自动遍历，各自的 :type
     走通用分支，不需要手动处理

   改名后立即检查顶层名字冲突——不同命名空间的同名符号在 :strip
   策略下会撞名，必须报错而非静默覆盖。"
  (:refer-clojure :exclude [apply])
  (:require
    [top.kzre.homunculus.backend.clang.lang :as clang.lang]
    [top.kzre.homunculus.backend.shader.ast :as ast]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

;; ═══════════════════════════════════════════
;; 符号改名
;; ═══════════════════════════════════════════

(defn- rename-symbol
  "把可能带命名空间的符号转为 GLSL 合法标识符。

   :strip  —— 只取 name 部分（cname 已剥离命名空间并替换非法字符）
   :mangle —— 全限定名整体转义（cname 接收全名字符串，替换 / 和 .）"
  [x strategy]
  (when-not (symbol? x)
    (throw (ex-info "expected symbol at ShaderAST name field"
                    {:value x :type (type x)})))
  (case strategy
    :strip  (symbol (clang.lang/naming x))
    :mangle (symbol (clang.lang/naming x false))
    (throw (ex-info "unknown ns-strategy" {:strategy strategy}))))

;; ═══════════════════════════════════════════
;; 类型改名
;; ═══════════════════════════════════════════

(defn- rename-type
  "递归改名 IR 类型里的 TCon 符号。

   TCon —— 改 name 符号（type-sym 命中）
   TVec —— 递归元素类型
   TFun —— 递归参数和返回
   其他 —— 原样（原始类型、字符串类型、nil）"
  [ir-type strategy]
  (cond
    (nil? ir-type)
    ir-type

    (ty/type-sym ir-type)
    (ty/make-tcon (rename-symbol (ty/type-sym ir-type) strategy))

    (ty/vec-type? ir-type)
    (assoc ir-type
      :element
      (rename-type (ty/vec-element-type ir-type) strategy))

    (ty/fun-type? ir-type)
    (ty/make-fun-type
      (mapv #(rename-type % strategy) (ty/fun-params ir-type))
      (rename-type (ty/fun-return-type ir-type) strategy))

    :else
    ir-type))

;; ═══════════════════════════════════════════
;; 遍历改名
;; ═══════════════════════════════════════════

(declare rename-node)

(defn- rename-node
  [node strategy]
  (let [kind (ast/kind node)
        ;; 先递归处理子节点——reduce-children 重建节点
        node (first
               (ast/reduce-children node
                                    (fn [child _] [(rename-node child strategy) nil])
                                    nil))]
    ;; 再处理自己的字段
    (cond-> node
            ;; 名字
            (some? (:name node))
            (update :name rename-symbol strategy)

            ;; 函数调用
            (and (= kind :call) (some? (:fn node)))
            (update :fn rename-symbol strategy)

            ;; 单值类型字段——:var-decl / :cast / :constructor /
            ;; :uniform / :static-var / :param / :struct-member 等
            (some? (:type node))
            (update :type rename-type strategy)

            ;; 函数 / 入口点返回类型
            (some? (:return-type node))
            (update :return-type rename-type strategy)

            ;; 纹理元素类型——影响 sampler 类型推导
            (some? (:element-type node))
            (update :element-type rename-type strategy))))

;; ═══════════════════════════════════════════
;; 冲突检测
;; ═══════════════════════════════════════════

(def ^:private global-nodes
  #{:uniform :static-var :function :texture-decl :sampler-decl
    :cbuffer-decl :struct :entry-point})

(defn- global-name
  [node]
  (when (contains? global-nodes (ast/kind node))
    (:name node)))

(defn- check-conflicts
  [asts]
  (let [names (keep global-name asts)
        dups  (->> names
                   frequencies
                   (filter #(> (val %) 1))
                   vec)]
    (when (seq dups)
      (throw (ex-info "name conflict after namespace strategy"
                      {:conflicts dups})))))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn apply
  "对 asts 应用命名空间策略。返回改名后的 asts。

   改名后立即检查顶层名字冲突。冲突时不返回部分结果——
   整个编译失败，避免后续流程基于有歧义的名字工作。"
  [asts strategy]
  (let [renamed (mapv #(rename-node % strategy) asts)]
    (check-conflicts renamed)
    renamed))