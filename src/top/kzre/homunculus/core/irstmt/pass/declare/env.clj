(ns top.kzre.homunculus.core.irstmt.pass.declare.env
  "前向收集使用，后向构造声明。

   ── 两个集合 ──

   ▸ bound —— 变量被赋值（def），bind-var 加入
   ▸ used  —— 变量被使用（use），use-var 加入

   unbound-var? 只看 bound——变量被 use 过不代表有声明。
   bind-var 的重复检查只看 bound——use 不阻止 bind。

   ── 为什么 use 不加入 bound ──

   后向遍历时，ret 或后续 stmt 的 use 先于 def 被看到。
   如果 use 加入 bound，def 时 unbound-var? 返回 false——
   不转 VarDecl——变量没有声明。use 必须加入独立的 used
   集合。")

(defprotocol IEnv
  (bind-var
    [_ var-name]
    "绑定局部变量（def），返回新环境。bound 重复报错。")
  (use-var
    [_ var-name]
    "使用变量（use），返回新环境。加入 used，幂等。
     不影响 bound——不阻止后续 bind-var。")
  (unbound-var?
    [_ var-name]
    "变量是否未绑定（没有 bind）。只看 bound。"))

(defrecord Env [bound used]
  IEnv
  (bind-var [_ var-name]
    (when (contains? bound var-name)
      (throw (ex-info "variable already bound"
                      {:var-name var-name :bound bound})))
    (->Env (conj bound var-name) used))
  (use-var [this var-name]
    (if (contains? used var-name)
      this
      (->Env bound (conj used var-name))))
  (unbound-var? [_ var-name]
    (not (contains? bound var-name))))

(defn make-env
  ([] (->Env #{} #{}))
  ([bound used] (->Env (or bound #{}) (or used #{}))))