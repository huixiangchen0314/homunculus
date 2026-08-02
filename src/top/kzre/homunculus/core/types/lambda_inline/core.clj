(ns top.kzre.homunculus.core.types.lambda-inline.core
  "Lambda 内联 Pass：消除 let 绑定的局部 lambda。
   使用递归 + case 特判，reduce-children 处理通用容器。"
  (:require
    [clojure.walk :as walk]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.protocol :as p]
    [top.kzre.homunculus.core.types.lambda-inline.protocol :as lp]
    [top.kzre.homunculus.core.types.free-vars :as free-vars]
    [top.kzre.homunculus.core.types.subst.api :as subst]))

;; ── 辅助函数（保持不变） ──
(defn has-non-call-usage?
  [body var-name]
  (let [call-fn-nodes (atom #{})   ;; 存储所有作为函数被调用的变量节点
        found         (atom false)]
    (walk/prewalk
      (fn [node]
        (when (satisfies? p/INode node)
          (let [kind (n/kind node)]
            ;; 将 :call 节点的 call-fn 子节点记录到集合
            (when (= kind :call)
              (swap! call-fn-nodes conj (n/call-fn node)))
            ;; 遇到目标变量节点且不在调用函数集合中，则标记为非调用使用
            (when (and (= kind :variable)
                       (= (n/var-name node) var-name)
                       (not (contains? @call-fn-nodes node)))
              (reset! found true))))
        node)
      body)
    @found))

(defn- collect-call-sites [body var-name]
  (let [sites (atom [])]
    (walk/prewalk
      (fn [node]
        (when (and (satisfies? p/INode node)
                   (= (n/kind node) :call))
          (let [fn-node (n/call-fn node)]
            (when (and (= (n/kind fn-node) :variable)
                       (= (n/var-name fn-node) var-name))
              (swap! sites conj node))))
        node)
      body)
    @sites))

(defn- inline-candidate? [lam config]
  (and (lp/should-inline? config lam nil)
       (let [size (count (tree-seq coll? seq (n/lambda-body lam)))]
         (<= size (lp/max-inline-size? config)))))

(defn- inline-call-site [call-node lambda-node]
  (subst/inline-call call-node lambda-node nil))

(defn- replace-call-sites [body var-name lambda-node]
  (let [sites (collect-call-sites body var-name)]
    (reduce (fn [cur-body site]
              (let [inlined (inline-call-site site lambda-node)]
                (walk/prewalk-replace {site inlined} cur-body)))
            body sites)))

(defn inline-let
  "如果 let 绑定的 lambda 满足条件，将其内联到 body 中所有调用点。"
  [let-node config]
  (let [bindings (n/let-bindings let-node)
        body     (n/let-body let-node)]
    (loop [remaining bindings
           new-bindings []
           current-body body]
      (if-let [[var val] (first remaining)]
        (if (and (= (n/kind val) :lambda)
                 (empty? (free-vars/free-vars-of-lambda val))
                 (inline-candidate? val config)
                 (not (has-non-call-usage? current-body (n/var-name var))))
          (let [new-body (replace-call-sites current-body (n/var-name var) val)]
            (recur (rest remaining) new-bindings new-body))
          (recur (rest remaining) (conj new-bindings [var val]) current-body))
        (n/make-let new-bindings current-body
                    (n/attrs let-node) (n/node-meta let-node))))))

(declare walk)

(defn- inline-fn [node config]
  (case (n/kind node)
    ;; 特判：let 节点需要先递归子节点，再尝试内联
    :let
    (let [processed (first (walk node config))]
      [(inline-let processed config) config])
    (walk node config)))

(defn walk
  [node config]
  (p/reduce-children node inline-fn config))

;; ── 入口 ──
(defn inline-nodes [ir2-roots config]
  (mapv #(inline-fn % config) ir2-roots))