(ns top.kzre.homunculus.core.irstmt.pass.assign-propagate.env
  "Assign-Propagate 环境。

   两个组件：
   - def-counts: 函数级预扫描结果（scan 输出），不可变
   - substs:     当前的变量别名表（x → y），在直线代码内传播

   作用域规则：
   - block 内建立的 substs 退出时丢弃
   - if 两个分支从同一 substs 出发，均不泄漏到 if 之后
   - while body 的 substs 不泄漏（循环可能执行 0 次）

   可变类型约束：
   SSA 消除本质是\"变量替换\"——把 use(x) 换成 use(y)。在值类型（不可变）
   下这是安全的纯拷贝。但对可变类型（shader 里 TVec → 数组），
   `x = y` 后若 `y` 被 aget/aset 修改，替换会让 x 的 use 看到被修改后的值，
   语义错误。因此 mutable-variable? 保守返回 true 时禁止 SSA 消除。"
  (:require
   [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
   [top.kzre.homunculus.core.ir2.pass.type :as ty]
   [top.kzre.homunculus.core.irstmt.pass.assign-propagate.scan :as scan]
   [top.kzre.homunculus.internal.protocol :as ip]))

(defprotocol IEnv
  (def-count  [env name]        "变量在函数内的定义次数")
  (ssa?       [env name]        "def-count == 1")
  (subst      [env name]        "查询变量当前替换目标，nil 表示无")
  (add-subst  [env name target] "建立 name → target 映射")
  (remove-subst [env name]      "移除 name 的映射（name 被重新定义）")
  (with-substs [env substs]     "替换整个 substs（进入/退出作用域）")
  (substs-of  [env]             "查询当前 substs（用于保存/恢复）")
  (mutable-value? [env node]
    "查询节点类型是否可变值。
    一共三种情况
    1. 引用
    2. 不可变值
    3. 可变值
    其中前面两种都可以安全的传播"))

(defrecord Env [ctx def-counts substs]
  IEnv
  (def-count [_ n] (get def-counts n 0))
  (ssa?      [_ n] (= 1 (get def-counts n 0)))
  (subst     [_ n] (get substs n))
  (add-subst [this n t] (assoc-in this [:substs n] t))
  (remove-subst [e n] (assoc e :substs (dissoc (:substs e) n)))
  (with-substs [e s] (assoc e :substs s))
  (substs-of [_] substs)
  (mutable-value? [_ node]
    (if-let [t (ty/get-type node)]
      (let [fe (ip/frontend ctx)]
        (tp/mutable-value-type? fe t))
      ;; 类型未推断 → 保守为可变，禁止 SSA 消除
      true)))

(defn make-env
  ([ctx] (->Env ctx {} {}))
  ([ctx def-counts] (->Env ctx def-counts {})))

(defn with-function-scope
  "为 function / method 节点创建新的子环境：
   - 继承 ctx（同一编译上下文）
   - 重新扫描该节点的 def-counts（新作用域）
   - substs 初始为空"
  [env node]
  (->Env (:ctx env) (scan/scan node) {}))