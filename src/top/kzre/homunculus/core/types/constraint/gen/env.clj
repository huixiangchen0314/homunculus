(ns top.kzre.homunculus.core.types.constraint.gen.env)

(defprotocol IEnv
  "约束生成环境。不可变——所有修改返回新环境。

   封装本地绑定和全局符号表——
   调用方不需要知道查到的是来自本地还是全局。
   lookup 内部先查本地，本地没有则查全局。"

  ;; ── 查询 ──
  (lookup [this name]
    "查找名字绑定的类型方案。
     本地优先，全局兜底。
     找不到返回 nil。")

  ;; ── 构造——不可变，返回新环境 ──
  (bind [this name scheme]
    "在当前作用域绑定名字到类型方案。
     返回新环境。")

  (bind-many [this bindings]
    "批量绑定。bindings 是 name→scheme 的 map。
     返回新环境。")

  ;; ── 类型变量 ──
  (fresh-tvar [this]
    "生成新的类型变量。
     返回 [new-env tvar]——因为计数器要推进。
     同一初始环境、同一求值顺序——结果确定。")

  ;; ── 深度追踪 ──
  (depth [this]
    "当前内联深度。")

  (with-depth [this n]
    "设置内联深度。返回新环境。")

  (max-depth [this]
    "最大内联深度。"))