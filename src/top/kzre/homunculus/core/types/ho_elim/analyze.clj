(ns top.kzre.homunculus.core.types.ho-elim.analyze
  "高阶分析：遍历 IR2 树，标记高阶函数定义。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.protocol :as p]))

(defn high-order?
  "若 lambda 的某个参数在其函数体内被用作调用目标，则返回 true。"
  [node]
  (if (= :lambda (p/kind node))
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
        (boolean (free-var-call? (n/lambda-body node)))))
    false))

(declare walk)
(defn- ho-fn
  "处理单个节点：如果是 define 且其值为 lambda，则根据高阶检测设置 :ho? 属性。
   其他节点原样返回。"
  [node env]
  (if (= :define (p/kind node))
    (let [new-val (first (ho-fn (n/define-val node) env))
          ho? (high-order? new-val)]
      [(n/make-define (n/define-name node)
                      new-val
                      (n/define-doc node)
                      (if ho?
                        (assoc (n/attrs node) :ho? true)
                        (n/attrs node))
                      (n/node-meta node)
                      )
       env])
    (walk node env)))

(defn walk
  [node env]
  (p/reduce-children node ho-fn env))


;; ── 对外入口 ─────────────────────────────────
(defn analyze
  "递归分析单个 IR2 节点（或根列表），标记所有高阶 define。
   若传入向量，则对每个元素应用 mark-ho。"
  [nodes]
  (mapv #(first (ho-fn % nil)) nodes))