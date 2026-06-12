(ns top.kzre.homunculus.core.types.inline-lift.protocol)

(defprotocol IInlineLiftConfig
  "内联/提升的配置协议。"
  (should-inline? [this lambda-node call-site]
    "根据 lambda 节点和调用点决定是否内联。返回 true/false。")
  (should-lift? [this lambda-node]
    "根据 lambda 节点决定是否提升到顶层。返回 true/false。")
  (max-inline-size [this]
    "返回允许内联的最大体大小（节点数）。")
  (lift-name-gen [this lambda-node]
    "为提升的函数生成顶层名称。"))