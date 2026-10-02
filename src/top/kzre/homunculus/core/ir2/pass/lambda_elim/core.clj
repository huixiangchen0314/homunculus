(ns top.kzre.homunculus.core.ir2.pass.lambda-elim.core
  "闭包消除核心：仅使用单态化（提升 + 特化）。通过多方法递归遍历 IR2。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.ast :as ir2p]
    [top.kzre.homunculus.core.ir2.analysis.free-vars :as free-vars]
    [top.kzre.homunculus.core.ir2.pass.lambda-elim.env :as p]))

(defn free-vars-of-lambda [lam env]
  (let [bound (p/bound-vars env)]
    (filter #(contains? bound %) (free-vars/free-vars-of-lambda lam))))

(defmulti elim-node*
          (fn [node _env] (n/kind node)))

(defn elim-fn
  [node {:keys [env defines]}]
  (let [[new-node defs] (elim-node* node env)]
    [new-node
     {:env env
      :defines (into defines defs)}]))

(defmethod elim-node* :default [node env]
  (let [[new-node {:keys [defines]}] (ir2p/reduce-children node elim-fn
                                                           {:env env
                                                            :defines []})]
    [new-node defines]))

;; has-lambda? 修复：递归检查所有子节点，不排除 define 的值
(defn has-lambda? [node]
  (if (ir2p/ir2? node)
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

(defn elim-node
  "对单个节点做闭包消除。"
  [node env]
  (elim-node* node env))

(defn elim [nodes env]
  (let [max-iter (p/max-iterations env)
        strict?  (p/strict-mode? env)]
    (loop [roots nodes iter 0]
      (let [[new-roots _]
            (reduce (fn [[new-roots defs] root]
                      (let [[new-root root-defs] (elim-node* root env)]
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