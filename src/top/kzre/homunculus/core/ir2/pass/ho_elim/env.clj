(ns top.kzre.homunculus.core.ir2.pass.ho-elim.env
  "ho-elim pass 的环境。不可变。

   两层职责：
   - 积累：追踪本地发现的高阶 lambda
   - 查询：统一查找——内部处理本地 / 全局的差异

   外部只看到「查找高阶 lambda」——不区分本地还是全局。
   编译上下文、符号表在外不可见——它们在查询内部被使用。"
  (:require
    [top.kzre.homunculus.core.symbol :as sym]
    [top.kzre.homunculus.internal.protocol :as ip]))

(defprotocol IEnv
  ;; ── 积累 ──
  (track-ho [this name lam]
    "追踪本地高阶 lambda。返回新环境。")

  ;; ── 查询 —— 统一入口 ──
  (lookup-ho [this name]
    "查找高阶 lambda。本地优先，全局兜底。
     返回 lambda 节点或 nil。
     外部不需要知道结果来自本地还是全局。")

  ;; ── 遍历状态 ──
  (depth [this])
  (with-depth [this n])
  (max-depth [this]))

(defrecord Env [ho-lambdas ho-names ctx depth-limit current-depth]
  IEnv
  (track-ho [_ name lam]
    (->Env (assoc ho-lambdas name lam)
           (conj ho-names name)
           ctx depth-limit current-depth))

  (lookup-ho [_ name]
    (or (when (contains? ho-names name)
          (get ho-lambdas name))
        (when-let [entry (sym/lookup-func (ip/symbol-table ctx) name)]
          (when (:ho? entry)
            (:ir2 entry)))))

  (depth      [_] current-depth)
  (with-depth [_ n]
    (->Env ho-lambdas ho-names ctx depth-limit n))
  (max-depth  [_] depth-limit))

(defn make-env [ctx]
  (->Env {} #{} ctx 20 0))