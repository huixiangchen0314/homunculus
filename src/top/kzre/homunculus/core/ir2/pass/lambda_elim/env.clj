(ns top.kzre.homunculus.core.ir2.pass.lambda-elim.env)

(defprotocol IEnv
  "Lambda 提升/特化的配置"
  (max-iterations [this]
    "最大迭代次数，默认为 5")
  (strict-mode? [this]
    "如果迭代结束仍有闭包，是否抛出异常")
  (on-unresolved [this lambda reason]
    "当仍有未解决的闭包时，回调通知（比如记录警告）")
  (lift-name-gen [this lambda-node]
    "根据 lambda 节点生成一个唯一的提升函数名")
  (bound-vars [this ] "返回环境中绑定的变量")
  (bind-var [this var-name] "绑定变量名"))


(defrecord Env [vars]
  IEnv
  (max-iterations [_] 1000)
  (strict-mode? [_] true)
  (on-unresolved [_ lambda _reason]
    (throw (ex-info "Unresolved closure" {:lambda lambda})))
  (lift-name-gen [_ _lambda]
    (symbol (str "lifted_" (gensym "lambda"))))
  (bound-vars [_] vars)
  (bind-var [this var-name] (assoc this :vars (conj vars var-name))))

(defn make-env []
  (->Env #{}))