(ns top.kzre.homunculus.core.ir2.pass.lambda-inline.env)

(defprotocol IEnv
  "Lambda 内联的配置。"
  (should-inline? [this lambda-node call-site]
    "根据 lambda 节点和调用点（可能为 nil）决定是否内联此 lambda。
     call-site 是调用该 lambda 的 CallNode 或 nil（例如在 let 绑定中）。")
  (max-inline-size [this]
    "返回允许内联的最大 AST 节点数，超过此阈值的内联将被拒绝。"))


(defrecord Env
  [^int max-inline-size]
  IEnv
  (max-inline-size [_] max-inline-size)
  (should-inline? [_ lambda-node call-site] true))

(defn make-env [& {:keys [max-inline-size]
                   :or {max-inline-size 1024}}]
  (->Env max-inline-size))