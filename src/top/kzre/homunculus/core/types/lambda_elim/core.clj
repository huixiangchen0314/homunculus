(ns top.kzre.homunculus.core.types.lambda-elim.core
  "闭包消除核心：仅使用单态化（提升 + 特化）。通过多方法递归遍历 IR2。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.model :as ir2p]
    [top.kzre.homunculus.core.types.free-vars :as free-vars]
    [top.kzre.homunculus.core.types.lambda-elim.protocol :as p]))

(defn free-vars-of-lambda [lam scoped-vars]
  (let [scoped? (fn [var] (contains? scoped-vars var))]
    (filter scoped? (free-vars/free-vars-of-lambda lam))))

(defmulti eliminate
          (fn [node  _config _env] (n/kind node)))

(defn elim-fn
  [node {:keys [config env defines]}]
  (let [[new-node defs] (eliminate node config env)]
    [new-node
     {:config config
      :env env
      :defines (into defines defs)}]))

(defmethod eliminate :default [node config env]
  (let [[new-node {:keys [defines]}] (ir2p/reduce-children node elim-fn
                                      {:config config
                                       :env env
                                       :defines []})]
    [new-node defines]))


;; has-lambda? 修复：递归检查所有子节点，不排除 define 的值
(defn has-lambda? [node]
  (if (satisfies? ir2p/INode node)
    (let [kind (n/kind node)]
      (case kind
        :lambda true
        :define (if (-> node n/attrs :ho?)
                  false   ;; 高阶函数跳过，交给 ho-elim 内联
                  (if-let [val (n/define-val node)]
                    (if (= :lambda (n/kind val))
                      (has-lambda? (n/lambda-body val))
                      (has-lambda? val))
                    false))
        (some has-lambda? (n/children node))))
    false))


(defn elim-nodes [nodes config]
  (let [max-iter (p/max-iterations config)
        strict?  (p/strict-mode? config)
        empty-env #{}]
    (loop [roots nodes iter 0]
      (let [[new-roots _]
            (reduce (fn [[new-roots defs] root]
                      (let [[new-root root-defs] (eliminate root config empty-env)]
                        [(-> new-roots (into root-defs) (conj new-root))
                         (into defs root-defs)]))
                    [[] []]
                    roots)
            all-roots new-roots]
        (if (some has-lambda? all-roots)
          (if (>= iter max-iter)
            (if strict?
              (throw (ex-info "Unable to eliminate all closures" {}))
              all-roots)
            (recur all-roots (inc iter)))
          all-roots)))))