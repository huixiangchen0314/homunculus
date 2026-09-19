(ns top.kzre.homunculus.core.ir2.analysis.ho
  "高阶函数分析。判断 lambda 是否为高阶。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.ast :as p]))

(defn ho?
  "若 lambda 的某个参数在其函数体内被用作调用目标，则返回 true。

   严格模式：仅接受 lambda 节点。其他节点抛异常。"
  [node]
  (when-not (= :lambda (p/kind node))
    (throw (ex-info "ho? expects a lambda node"
                    {:node node
                     :kind (p/kind node)})))
  (let [param-names (set (map n/var-name (n/lambda-params node)))]
    (letfn [(free-var-call? [node]
              (cond
                (n/call-node? node)
                (let [fn-node (n/call-fn node)]
                  (or (and (n/variable-node? fn-node)
                           (contains? param-names (n/var-name fn-node)))
                      (some free-var-call? (n/children node))))
                :else
                (some free-var-call? (n/children node))))]
      (boolean (free-var-call? (n/lambda-body node))))))