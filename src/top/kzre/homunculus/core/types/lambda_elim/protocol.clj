(ns top.kzre.homunculus.core.types.lambda-elim.protocol)

(defprotocol ILiftConfig
  "Lambda 提升/特化的配置"
  (max-iterations [this]
    "最大迭代次数，默认为 5")
  (strict-mode? [this]
    "如果迭代结束仍有闭包，是否抛出异常")
  (on-unresolved [this lambda reason]
    "当仍有未解决的闭包时，回调通知（比如记录警告）")
  (lift-name-gen [this lambda-node]
    "根据 lambda 节点生成一个唯一的提升函数名"))