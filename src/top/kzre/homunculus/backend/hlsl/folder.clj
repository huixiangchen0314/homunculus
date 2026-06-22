(ns top.kzre.homunculus.backend.hlsl.folder
  "HLSL 常量折叠器：实现 IFolder 协议，对编译时可求值的表达式进行常量折叠。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.fold.protocol :as fp]))

;; ── 多方法处理 call 节点 ──
(defmulti fold-call (fn [fn-name _args _context] fn-name))

(defmethod fold-call :default [_ _ _] nil)

(defn- eval-binary-op [op lv rv]
  (try
    (case op
      +   (+ lv rv)
      %%+ (+ lv rv)
      -   (- lv rv)
      *   (* lv rv)
      /   (if (zero? rv) (throw (ex-info "Division by zero" {})) (/ lv rv))
      <   (< lv rv)
      >  (> lv rv)
      <= (<= lv rv)
      >= (>= lv rv)
      =  (= lv rv)
      not= (not= lv rv))
    (catch Exception _ nil)))

(defn- fold-binary [fn-name args _context]
  (when (= 2 (count args))
    (let [[left right] args]
      (when (and (n/literal-node? left) (n/literal-node? right))
        (when-let [result (eval-binary-op fn-name (n/lit-val left) (n/lit-val right))]
          (let [result-node (n/make-literal result nil nil)]
            result-node))))))

(doseq [op ['+ '%%+ '- '* '/ '< '> '<= '>= '= 'not=]]
  (defmethod fold-call op [fn-name args context]
    (fold-binary fn-name args context)))

(defrecord HLSLFolder []
  fp/IFolder
  (fold-node [_ node context]
    (when (n/call-node? node)
      (let [fn-node (n/call-fn node)]
        (when (n/variable-node? fn-node)
          (fold-call (n/var-name fn-node) (n/call-args node) context))))))

(defn folder
  []
  (->HLSLFolder))