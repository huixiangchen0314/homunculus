(ns top.kzre.homunculus.core.ir2.pass.lambda-inline.core
  "Lambda 内联 Pass：消除 let 绑定的局部 lambda。
   使用递归 + case 特判，reduce-children 处理通用容器。"
  (:require
    [clojure.walk :as walk]
    [top.kzre.homunculus.core.ir2.analysis.free-vars :as free-vars]
    [top.kzre.homunculus.core.ir2.analysis.var-usage :as var-usage]
    [top.kzre.homunculus.core.ir2.ast :as p]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.lambda-inline.env :as lp]
    [top.kzre.homunculus.core.ir2.transform.inline :as inline]))


(defn- inline-candidate? [lam config]
  (and (lp/should-inline? config lam nil)
       (let [size (count (tree-seq coll? seq (n/lambda-body lam)))]
         (<= size (lp/max-inline-size config)))))

(defn- replace-call-sites [body var-name lambda-node]
  (let [sites (var-usage/call-sites body var-name)]
    (reduce (fn [cur-body site]
              (let [inlined (inline/inline-call site lambda-node)]
                (walk/prewalk-replace {site inlined} cur-body)))
            body
            sites)))

(defn inline-let
  "如果 let 绑定的 lambda 满足条件，将其内联到 body 中所有调用点。"
  [let-node config]
  (let [bindings (n/let-bindings let-node)
        body     (n/let-body let-node)]
    (loop [remaining bindings
           new-bindings []
           current-body body]
      (if (seq remaining)
        (let [b (first remaining)
              var-node (:var b)
              val-node (:val b)]
          (if (and (= (:kind val-node) :lambda)
                   (empty? (free-vars/free-vars-of-lambda val-node))
                   (inline-candidate? val-node config)
                   (not (var-usage/non-call-usage? current-body (:name var-node))))
            (recur (rest remaining)
                   new-bindings
                   (replace-call-sites current-body (:name var-node) val-node))
            (recur (rest remaining)
                   (conj new-bindings b)
                   current-body)))
        ;; 所有绑定处理完毕
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
(defn inline [ir2-roots config]
  (mapv #(inline-fn % config) ir2-roots))