(ns top.kzre.homunculus.core.ir2.pass.module.env
  "模块解析的环境。保存命名空间、别名、局部绑定、全局定义集合。
   所有操作返回新记录——不修改原值。

   ── 符号解析规则 ──────────────────────────────────────
   1. 带命名空间的符号——前缀是别名则替换为完整命名空间；
      否则保持原样（已是完整名或未知命名空间）。
   2. 局部绑定——保持原名（不加前缀）。
   3. 别名（无命名空间的别名符号）——替换为别名目标。
   4. 全局定义——加当前模块命名空间前缀。
   5. 其它——保持原名——未定义符号，错误留给后续阶段。"
  (:require
    [top.kzre.homunculus.core.error :as err]
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.pass.namespace :as namespace]
    [top.kzre.homunculus.core.ir2.pass.protocol :as types]
    [top.kzre.homunculus.internal.protocol :as ip]))

(defprotocol IEnv
  (module-ns [_] "当前环境的命名空间")
  (add-global-def [_ var-name] "绑定模块全局符号到命名空间")
  (add-local-def [_ var-name])
  (resolve-symbol [_ var-name]))

(defrecord Env [ns-name aliases bound-vars global-defs]
  IEnv
  (module-ns [_]
    ns-name)

  (add-global-def [this var-name]
    (assoc this :global-defs (conj global-defs var-name)))

  (add-local-def [this var-name]
    (assoc this :bound-vars (conj bound-vars var-name)))

  (resolve-symbol [_ var-name]
    (or (cond
          ;; 1. 带命名空间——别名替换
          (namespace var-name)
          (when-let [full-ns (get aliases (symbol (namespace var-name)))]
            (symbol (str full-ns) (name var-name)))

          ;; 2. 局部绑定——返回 nil 让 or 回退到原名
          (contains? bound-vars var-name)
          nil

          ;; 3. 别名符号
          (contains? aliases var-name)
          (get aliases var-name)

          ;; 4. 全局定义——加模块前缀
          (contains? global-defs var-name)
          (symbol (str ns-name) (name var-name))

          ;; 5. 其它
          :else nil)
        var-name)))

(defn bind-vars
  "批量添加局部绑定。"
  [e var-names]
  (reduce add-local-def e var-names))

;; ── 构造辅助 ──────────────────────────────────────────

(defn- extract-ns-node
  "从模块根节点提取唯一的 ns 声明。数量不为 1 时抛异常。"
  [ir2-roots]
  (let [ns-nodes (filter #(= :ns (ir2/kind %)) ir2-roots)
        cnt      (count ns-nodes)]
    (cond
      (zero? cnt)
      (throw (err/compile-error
               :missing-ns-declaration
               "No namespace declaration found"
               {}))

      (> cnt 1)
      (throw (err/compile-error
               :multiple-ns-declarations
               (str "Multiple namespace declarations found: " cnt)
               {:count cnt}))

      :else (first ns-nodes))))

(defn- register-dep-syms!
  "向 compile-ctx 注册 ns 声明的依赖符号。
   非 cljh.core 模块自动加 cljh.core 兜底依赖。"
  [ns-node compile-ctx]
  (let [self-ns  (:name ns-node)
        frontend (ip/frontend compile-ctx)
        macro-ns (or (when frontend (types/macro-namespaces frontend)) #{})
        dep-syms (cond-> (remove macro-ns
                                 (namespace/ns-dependency-syms ns-node))
                         (not= self-ns 'cljh.core)
                         (conj 'cljh.core))]
    (ip/register-deps compile-ctx dep-syms)))

(defn- collect-aliases
  "计算别名表——用户声明的引用别名 + cljh.core 的标准导出。"
  [ns-node compile-ctx]
  (let [sym-table    (ip/symbol-table compile-ctx)
        user-aliases (namespace/ns-reference-aliases ns-node sym-table)
        std-aliases  (namespace/ns-exported-syms sym-table 'cljh.core)]
    (merge user-aliases std-aliases)))

(defn build-env
  "从模块根节点和 compile-ctx 构造初始 Env。

   校验：模块必须恰好一个 ns 声明。
   副作用：向 compile-ctx 注册依赖符号（含 cljh.core 兜底）。"
  [ir2-roots compile-ctx]
  (let [ns-node (extract-ns-node ir2-roots)
        _       (register-dep-syms! ns-node compile-ctx)
        aliases (collect-aliases ns-node compile-ctx)]
    (->Env (:name ns-node) aliases #{} #{})))