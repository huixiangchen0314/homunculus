(ns top.kzre.homunculus.core.ir2.transform.rename.env
  "Alpha 重命名的环境。

   环境是「旧名 → 新名」的映射。
   随遍历扩展——进入 let / lambda / loop / catch 时加入绑定。
   离开时不回退——因为变换后的树已经用新名。

   协议 + 记录：环境对外只暴露操作，字段不可见。")

(defprotocol IEnv
  (bind [this old-name new-name]
    "把 old-name 绑定到 new-name。返回新环境。")
  (bind-all [this pairs]
    "批量绑定。pairs 是 [old-name new-name] 的序列。返回新环境。")
  (resolve-subst-name [this old-name]
    "解析 old-name 到 new-name。找不到返回 nil。"))

(defrecord Env [name-substs]
  IEnv
  (bind [_ old-name new-name]
    (->Env (assoc name-substs old-name new-name)))
  (bind-all [_ pairs]
    (->Env (into name-substs pairs)))
  (resolve-subst-name [_ old-name]
    (get name-substs old-name)))

(defn make-env []
  (->Env {}))