(ns top.kzre.homunculus.backend.hlsl.methods.define
  "HLSL :define 节点发射。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.hlsl.core :as core]
    [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]))


;; ── 辅助函数：将函数体转换为 HLSL 语句序列，最后一个表达式自动加 return ──

(defn- emit-fn-body
  "递归处理函数体节点，返回不含花括号的函数体字符串。
   处理 let 展开、block 序列，并保证最后一条语句是 return。"
  [node]
  (cond
    ;; let → 变量声明 + 继续处理 body
    (n/let-node? node)
    (let [bindings (n/let-bindings node)
          body     (n/let-body node)
          decls    (mapv (fn [[v e]]
                           (let [v-type (core/hlsl-type-str (ty/get-type v))
                                 v-name (name (n/var-name v))
                                 init   (core/emit-node e)]
                             (str v-type " " v-name " = " init ";")))
                         bindings)
          rest-str (emit-fn-body body)]
      (str (str/join "\n" decls) "\n" rest-str))

    ;; block → 前导语句正常发射，最后一条交给 emit-fn-body
    (n/block-node? node)
    (let [exprs (n/block-exprs node)
          inits (butlast exprs)
          last  (last exprs)]
      (str (str/join "\n" (mapv #(str (core/emit-node %) ";") inits))
           (when (seq inits) "\n")
           (emit-fn-body last)))

    ;; 其他 → 最终返回值
    :else
    (str "return " (core/emit-node node) ";")))


(defmethod core/emit-node :define [node]
  (let [type (ty/get-type node)]
    (cond
      (ty/fun-type? type)
      ;; 函数声明
      (let [lam        (or (n/define-val node)
                           (throw (ex-info "Define node has no value"
                                           {:node node :name (n/define-name node)})))
            fn-ty      (ty/get-type lam)
            ret-type   (if (ty/fun-type? fn-ty)
                         (core/hlsl-type-str (ty/fun-result fn-ty))
                         (throw (ex-info "Cannot determine return type"
                                         {:fn-name (n/define-name node)})))
            params     (n/lambda-params lam)
            param-strs (mapv (fn [p] (str (core/hlsl-type-str (ty/get-type p)) " " (name (n/var-name p)))) params)
            body       (n/lambda-body lam)
            ;; ---- 核心：用 emit-fn-body 处理函数体 ----
            body-str   (emit-fn-body body)
            func-name  (name (n/define-name node))]
        (str (tmpl/func-signature ret-type func-name (str/join ", " param-strs))
             "\n"
             (tmpl/func-body body-str)))   ;; func-body 只负责加花括号

      :else  ;; 变量声明
      (let [val (n/define-val node)
            type-str (core/hlsl-type-str (ty/get-type val))
            name-str (name (n/define-name node))
            init-str (core/emit-node val)]
        (tmpl/var-decl type-str name-str init-str)))))
