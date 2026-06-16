(ns top.kzre.homunculus.core.types.lambda-elim.core
  "闭包消除核心：仅使用单态化（提升 + 特化）。通过多方法递归遍历 IR2。"
  (:require
    [clojure.set :as set]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.lambda-elim.protocol :as p]
    [top.kzre.homunculus.core.types.free-vars :as free-vars]
    [top.kzre.homunculus.core.types.subst.api :as subst]
    [top.kzre.homunculus.core.types.utils :as u]))

;; ── 自由变量分析（委托给统一模块） ──
(defn free-vars-of-lambda [lam]
  (free-vars/free-vars-of-lambda lam))

;; ── 单态化：提升 + 特化 ────────────────
(defn monomorphize
  "将 call-node 中第 idx 个参数（lambda）提升为顶层函数，并特化目标函数。
   返回 {:new-call ... :new-defines [...]}，调用者负责将 new-defines 注册到全局。"
  [call-node lam idx ir2-roots config]
  (let [fn-name (n/var-name (n/call-fn call-node))
        fn-def  (u/find-toplevel-define ir2-roots fn-name)]
    (when-not fn-def (throw (ex-info "Target not found" {:name fn-name})))
    (let [target-lam    (n/define-val fn-def)
          target-params (n/lambda-params target-lam)
          _             (when (>= idx (count target-params))
                          (throw (ex-info "Invalid lambda parameter index" {:idx idx})))
          formal        (nth target-params idx)
          fv            (free-vars-of-lambda lam)

          ;; 1) 提升 lambda 为顶层定义
          lifted-name   (p/lift-name-gen config lam)
          ;; 新参数 = 原参数 + 自由变量
          new-params    (into (mapv #(n/make-variable (n/var-name %) nil nil) target-params)
                              (mapv #(n/make-variable % nil nil) fv))
          lifted-lam    (n/make-lambda new-params (n/lambda-body lam)
                                       (vec fv) nil
                                       (n/attrs lam) (n/node-meta lam) nil)

          ;; 2) 特化目标函数：将 formal 替换为对 lifted-name 的调用，并传入自由变量
          ;;    构造调用表达式: (lifted-name fv1 fv2 ...)
          call-replacement (n/make-call (n/make-variable lifted-name nil nil)
                                        (mapv #(n/make-variable % nil nil) fv)
                                        {} nil nil)
          specialized-body (subst/replace-var (n/lambda-body target-lam)
                                              (n/var-name formal)
                                              call-replacement)
          specialized-lam  (n/make-lambda target-params specialized-body
                                          (n/lambda-captures target-lam) nil
                                          (n/attrs target-lam) (n/node-meta target-lam) nil)
          specialized-name (u/fresh-name (symbol (str (n/define-name fn-def) "_" (name lifted-name))))

          ;; 3) 新调用点：替换原 call 中的 lambda 参数为提升后的函数名 + 自由变量
          new-args (assoc (n/call-args call-node) idx
                                                  (n/make-variable lifted-name nil nil))
          new-call (n/make-call (n/make-variable specialized-name nil nil)
                                new-args
                                (n/attrs call-node) (n/node-meta call-node) nil)]
      {:new-call new-call
       :new-defines [(n/make-define lifted-name lifted-lam nil nil nil nil)
                     (n/make-define specialized-name specialized-lam nil nil nil nil)]})))

;; ── 多方法 elimina te ────────────────────
(defmulti eliminate
          (fn [node _ir2-roots _config _new-defs] (n/kind node)))



(defn has-lambda? [node]
  (if (satisfies? ir2p/INode node)
    (case (n/kind node)
      :define false
      :lambda true
      (some has-lambda? (n/children node)))
    false))

(defn elaborate [ir2-roots config]
  (let [max-iter (p/max-iterations config)
        strict?  (p/strict-mode? config)]
    (loop [roots ir2-roots iter 0]
      (let [new-defs (atom [])
            new-roots (mapv #(eliminate % roots config new-defs) roots)
            all-roots (into new-roots @new-defs)]
        (if (some has-lambda? all-roots)
          (if (>= iter max-iter)
            (if strict?
              (throw (ex-info "Unable to eliminate all closures" {}))
              all-roots)
            (recur all-roots (inc iter)))
          all-roots)))))