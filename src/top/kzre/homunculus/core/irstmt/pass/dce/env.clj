(ns top.kzre.homunculus.core.irstmt.pass.dce.env
  "DCE 环境——基于集合的活跃变量分析。

   两个 live 集合：
   - live-read: 被读取的变量（旧值必须被计算）
   - live-ref:  被引用的变量（被 use 或作为 assign 的 target）
     live-read ⊆ live-ref

   二者区别：
   - use(x)        → live-read += x, live-ref += x
   - x = e (assign) → live-read -= x, live-ref += x  （旧值被 kill，但需保留声明）
   - int x (decl)   → live-read -= x, live-ref -= x  （新变量，之前的活需求失效）

   判定：
   - assign (简单变量 target)：target ∈ live-read-out → 保留
   - var-decl：name ∈ live-ref-out → 保留
   - 其他语句：由 fx? 决定"

  (:require
    [top.kzre.homunculus.internal.protocol :as proto]
    [top.kzre.homunculus.core.symbol :as sym]))

(defprotocol IEnv
  (use-var   [env name] "标记变量被读取")
  (def-var   [env name] "标记变量被定义（assign）：kill 读需求，保留引用需求")
  (decl-var  [env name] "标记变量声明（var-decl）：kill 读和引用需求")
  (live-read? [env name] "变量是否在 live-read 中")
  (live-ref?  [env name] "变量是否在 live-ref 中")
  (merge-branches [env1 env2] "合并 if 分支：live-read 和 live-ref 分别 union")
  (same-live? [env1 env2] "live-read 和 live-ref 是否都相等"))

(defrecord Env [ctx live-read live-ref]
  IEnv
  (use-var [_ name]
    (->Env ctx (conj live-read name) (conj live-ref name)))
  (def-var [_ name]
    (->Env ctx (disj live-read name) (conj live-ref name)))
  (decl-var [_ name]
    (->Env ctx (disj live-read name) (disj live-ref name)))
  (live-read? [_ name] (contains? live-read name))
  (live-ref?  [_ name] (contains? live-ref name))
  (merge-branches [e1 e2]
    (->Env ctx
           (into (:live-read e1) (:live-read e2))
           (into (:live-ref  e1) (:live-ref  e2))))
  (same-live? [e1 e2]
    (and (= (:live-read e1) (:live-read e2))
         (= (:live-ref  e1) (:live-ref  e2)))))

(defn make-env [ctx] (->Env ctx #{} #{}))

(defn fn-fx? [env fn-name]
  (let [tbl   (proto/symbol-table (:ctx env))
        entry (sym/lookup-sym tbl fn-name)]
    (sym/func-fx? entry)))