(ns top.kzre.homunculus.core.types.elaborate.protocol)

(defprotocol IElaborateConfig
  (max-iterations [this] "最大迭代次数，默认 5")
  (strict-mode? [this] "是否对无法消除的闭包抛出异常")
  (allow-return-closure? [this] "是否允许返回闭包（HLSL 必须 false）")
  (on-unresolved [this lambda] "无法消除闭包时的回调，默认抛出异常")
  (should-inline? [this lambda-node call-node] "根据 lambda 和调用点决定是否内联"))