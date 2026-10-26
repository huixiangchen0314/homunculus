(ns top.kzre.homunculus.core.ir2.pass.lambda-elim.core
  "闭包消除核心：仅使用单态化（提升 + 特化）。通过多方法递归遍历 IR2。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.ast :as ir2p]
    [top.kzre.homunculus.core.ir2.analysis.free-vars :as free-vars]
    [top.kzre.homunculus.core.ir2.pass.lambda-elim.env :as env]
    [top.kzre.homunculus.core.ir2.pass.mark-trait :as mark-trait]))

(defn free-vars-of-lambda [lam env]
  (let [bound (env/bound-vars env)]
    (filter #(contains? bound %) (free-vars/free-vars-of-lambda lam))))


(defn lift-and-replace-call [call-node lambda-node args env]
  (let [fv          (free-vars-of-lambda lambda-node env)
        fv-vec      (vec fv)
        lifted-name (env/lift-name-gen env lambda-node)
        new-params  (if (empty? fv-vec)
                      (n/lambda-params lambda-node)
                      (into (mapv #(n/make-variable (n/var-name %) nil nil)
                                  (n/lambda-params lambda-node))
                            (mapv #(n/make-variable % nil nil) fv-vec)))
        ;; ← 自由变量已经是函数参数，函数体内直接可见，不需要 let 重绑定
        new-body    (n/lambda-body lambda-node)
        lifted-lam  (n/make-lambda new-params new-body nil nil
                                   (n/attrs lambda-node) (n/node-meta lambda-node))
        new-call    (n/make-call (n/make-variable lifted-name nil nil)
                                 (if (empty? fv-vec)
                                   args
                                   (into args (mapv #(n/make-variable % nil nil) fv-vec)))
                                 (n/attrs call-node) (n/node-meta call-node))
        init-attrs
        (merge
          (n/attrs lambda-node)
          (if (env/inline-all-lambda? env) {:inline true} {}))
        ;; 继承lambda meta, 允许用户标记 lambda 内联
        init-meta (n/node-meta lambda-node)
        new-defines [(n/make-define
                       lifted-name lifted-lam nil
                       init-attrs init-meta)]]
    {:new-call    new-call
     ;; 对提升的函数重新分析特性
     :new-defines (mark-trait/mark new-defines)}))

(defmulti elim-node*
          (fn [node _env] (n/kind node)))

(defn elim-seq [nodes env]
  (reduce (fn [[nodes-acc defs-acc] n]
            (let [[new-node new-defs] (elim-node* n env)]
              [(conj nodes-acc new-node) (into defs-acc new-defs)]))
          [[] []]
          nodes))

(defn elim-fn
  [node {:keys [env defines]}]
  (let [[new-node defs] (elim-node* node env)]
    [new-node
     {:env env
      :defines (into defines defs)}]))

(defmethod elim-node* :default [node env]
  (let [[new-node {:keys [defines]}]
        (ir2p/reduce-children node elim-fn
                              {:env env
                               :defines []})]
    [new-node defines]))

(defn- has-closure?
  "节点是否存在闭包"
  [node]
  (if (ir2p/ir2? node)
    (let [kind (n/kind node)]
      (case kind
        :lambda true
        :define (if (mark-trait/high-order? node)
                  false   ;; 高阶函数跳过，交给 ho-elim 内联
                  (if-let [val (n/define-val node)]
                    (if (= :lambda (n/kind val))
                      (has-closure? (n/lambda-body val))
                      (has-closure? val))
                    false))
        (some has-closure? (n/children node))))
    false))

(defn elim-node
  "对单个节点做闭包消除。"
  [node env]
  (elim-node* node env))

(defn elim [nodes env]
  (let [max-iter (env/max-iterations env)
        strict?  (env/strict-mode? env)]
    (loop [roots nodes iter 0]
      (let [[new-roots _]
            (reduce (fn [[new-roots defs] root]
                      (let [[new-root root-defs] (elim-node* root env)]
                        [(-> new-roots (into root-defs) (conj new-root))
                         (into defs root-defs)]))
                    [[] []]
                    roots)
            all-roots new-roots]
        (if (some has-closure? all-roots)
          (if (>= iter max-iter)
            (if strict?
              (throw (ex-info "Unable to eliminate all closures" {}))
              all-roots)
            (recur all-roots (inc iter)))
          all-roots)))))