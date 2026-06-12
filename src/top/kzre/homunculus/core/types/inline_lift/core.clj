(ns top.kzre.homunculus.core.types.inline-lift.core
  (:require [top.kzre.homunculus.core.types.inline-lift.protocol :as cfg]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]
            [clojure.set :as set]))

;; ── 自由变量分析 ──
(defn free-vars [node]
  (let [bound (atom #{})
        _ (letfn [(collect [n]
                    (when (satisfies? ir2p/INode n)
                      (case (ir2p/kind n)
                        :let (doseq [[v _] (:bindings n)]
                               (swap! bound conj (:name v)))
                        :lambda (doseq [p (:params n)]
                                  (swap! bound conj (:name p)))
                        :loop (doseq [[v _] (:bindings n)]
                                (swap! bound conj (:name v)))
                        nil)
                      (doseq [c (ir2p/children n)] (collect c))))]
            (collect node))
        _ (println "free-vars: bound =" @bound)
        free (atom #{})
        _ (letfn [(walk [n]
                    (when (satisfies? ir2p/INode n)
                      (if (= (ir2p/kind n) :variable)
                        (do (println "  seeing var" (:name n) "bound?" (contains? @bound (:name n)))
                            (when-not (contains? @bound (:name n))
                              (swap! free conj (:name n))))
                        (doseq [c (ir2p/children n)] (walk c)))))]
            (walk node))]
    (println "free-vars: result =" @free)
    @free))

;; ── 内联 ──
(defn inline-call [call-node lambda-node config]
  (println "inline-call: lambda body =" (:body lambda-node))
  (let [params (:params lambda-node)
        args (:args call-node)
        body (:body lambda-node)
        subst (zipmap (map :name params) args)
        replace-var (fn replace-var [node]
                      (if (and (satisfies? ir2p/INode node)
                               (= (ir2p/kind node) :variable)
                               (contains? subst (:name node)))
                        (subst (:name node))
                        (if (satisfies? ir2p/INode node)
                          (let [new-children (mapv replace-var (ir2p/children node))]
                            (clojure.core/assoc node :children new-children))
                          node)))]
    (replace-var body)))

;; ── 提升 ──
(defn lift-lambda [lambda-node free-vars config]
  (println "lift-lambda: free-vars =" free-vars)
  (let [lifted-name (cfg/lift-name-gen config lambda-node)
        new-lambda (if (seq free-vars)
                     (let [extra-params (mapv #(m/->VariableNode % nil nil [] nil) free-vars)
                           let-bindings (vec (for [fv free-vars]
                                               [(m/->VariableNode fv nil nil [] nil)
                                                (m/->VariableNode fv nil nil [] nil)]))
                           new-body (m/->LetNode let-bindings (:body lambda-node) nil nil [] nil)]
                       (assoc lambda-node
                         :params (into (:params lambda-node) extra-params)
                         :body new-body))
                     lambda-node)
        define-node (m/->DefineNode lifted-name new-lambda nil nil nil [] nil)
        ref-node (m/->VariableNode (name lifted-name) nil nil [] nil)]
    {:define define-node :ref ref-node}))

;; ── 主遍历 ──
(defn transform [ir2-root config]
  (let [lifted (atom [])]
    (letfn [(walk [node]
              (if-not (satisfies? ir2p/INode node)
                node
                (let [kind (ir2p/kind node)]
                  (case kind
                    :call
                    (let [fn-node (:fn node)]
                      (if (= (ir2p/kind fn-node) :lambda)
                        (do (println "transform :call with lambda")
                            (println "  should-inline? " (cfg/should-inline? config fn-node node))
                            (if (cfg/should-inline? config fn-node node)
                              (do (println "  -> inline")
                                  (inline-call node fn-node config))
                              (do (println "  -> lift")
                                  (let [fv (free-vars fn-node)
                                        {:keys [define ref]} (lift-lambda fn-node fv config)]
                                    (swap! lifted conj define)
                                    (assoc node :fn ref)))))
                        (let [new-fn (walk fn-node)
                              new-args (mapv walk (:args node))]
                          (assoc node :fn new-fn :args new-args))))
                    (:let :block :loop :if :define :try :lambda)
                    (let [new-children (mapv walk (ir2p/children node))]
                      (clojure.core/assoc node :children new-children))
                    node))))]
      (let [new-root (walk ir2-root)]
        [new-root @lifted]))))

(defn eliminate-closures [ir2-roots config]
  (let [results (map #(transform % config) ir2-roots)
        new-roots (mapcat (fn [[root defs]] (cons root defs)) results)]
    new-roots))