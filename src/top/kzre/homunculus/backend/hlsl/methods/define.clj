(ns top.kzre.homunculus.backend.hlsl.methods.define
  "HLSL :define 节点发射。支持 let、block、while、if、assign 以及数组声明。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.hlsl.core :as core]
    [top.kzre.homunculus.backend.hlsl.render :as render]
    [top.kzre.homunculus.backend.util.naming :refer [cname]]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]))

;; ── 将子节点转换为单行表达式字符串 ──
(defn- expr-str [node context]
  (render/render-node (core/emit-node node context) 0))

;; ── 片段：一组前置声明 + 一个表达式 ──
(defn- make-fragment [stmts expr]
  {:stmts (vec stmts) :expr expr})

(defn- fragment-expr [expr]
  (make-fragment [] expr))

(defn- merge-fragments [& fragments]
  (let [stmts (mapcat :stmts fragments)
        last-expr (or (last (keep :expr fragments)) "")]
    (make-fragment stmts last-expr)))

;; ── 辅助：生成数组逐元素拷贝的 while 循环字符串 ──
(defn- emit-array-copy-loop [target-str val-str size-str]
  (let [idx (gensym "i")
        idx-name (str idx)
        indent "  "]
    (str "int " idx-name " = 0;\n"
         "while (" idx-name " < " size-str ")\n"
         "{\n"
         indent target-str "[" idx-name "] = " val-str "[" idx-name "];\n"
         indent "++" idx-name ";\n"
         "}")))

;; ── 将 IR 节点转换为片段 ─────────────────
(defn- node->fragment [node context]
  (let [kind (n/kind node)]
    (cond
      ;; let 节点
      (= kind :let)
      (let [bindings (n/let-bindings node)
            body     (n/let-body node)
            bind-frags (mapv (fn [[v e]]
                               (let [ir-type (ty/get-type v)]
                                 (if (ty/vec-type? ir-type)
                                   ;; 数组声明
                                   (let [size (ty/vec-size ir-type)
                                         elem-ty (core/hlsl-type-str (ty/vec-element-type ir-type))
                                         name-str (cname (n/var-name v))
                                         decl (str elem-ty " " name-str "[" size "];")]
                                     (make-fragment [] decl))
                                   ;; 普通变量
                                   (let [v-type (core/hlsl-type-str ir-type)
                                         name-str (cname (n/var-name v))
                                         frag-e (node->fragment e context)
                                         var-decl (str v-type " " name-str " = " (:expr frag-e) ";")]
                                     (make-fragment (:stmts frag-e) var-decl)))))
                             bindings)
            ordered-stmts (mapcat (fn [frag] (conj (vec (:stmts frag)) (:expr frag))) bind-frags)
            frag-body (node->fragment body context)]
        (make-fragment (concat ordered-stmts (:stmts frag-body))
                       (:expr frag-body)))

      (= kind :block)
      (let [exprs (n/block-exprs node)
            inits (butlast exprs)
            last  (last exprs)
            init-frags (mapv #(node->fragment % context) inits)
            stmt-stmts (mapcat (fn [frag]
                                 (concat (:stmts frag)
                                         (when (not (str/blank? (:expr frag)))
                                           [(str (:expr frag) ";")])))
                               init-frags)
            last-frag (node->fragment last context)]
        (merge-fragments (make-fragment stmt-stmts "")
                         last-frag))

      (= kind :while)
      (let [test-str (expr-str (n/while-test node) context)
            body-frag (node->fragment (n/while-body node) context)
            body-stmts (concat (:stmts body-frag)
                               (when (not (str/blank? (:expr body-frag)))
                                 [(:expr body-frag)]))
            while-str (str "while (" test-str ") {\n"
                           (str/join "\n" (map #(str "  " %) body-stmts))
                           "\n}")]
        (make-fragment [while-str] ""))

      (= kind :if)
      (let [test-str (expr-str (n/if-test node) context)
            then-frag (node->fragment (n/if-then node) context)
            else-frag (when-let [e (n/if-else node)]
                        (node->fragment e context))
            then-stmts (concat (:stmts then-frag)
                               (when (not (str/blank? (:expr then-frag)))
                                 [(:expr then-frag)]))
            else-stmts (when else-frag
                         (concat (:stmts else-frag)
                                 (when (not (str/blank? (:expr else-frag)))
                                   [(:expr else-frag)])))
            if-str (str "if (" test-str ") {\n"
                        (str/join "\n" (map #(str "  " %) then-stmts))
                        "\n}")
            full-str (if else-stmts
                       (str if-str " else {\n"
                            (str/join "\n" (map #(str "  " %) else-stmts))
                            "\n}")
                       if-str)]
        (make-fragment [full-str] ""))

      (= kind :assign)
      (let [target-node (n/assign-var node)
            target-type (ty/get-type target-node)]
        (if (ty/vec-type? target-type)
          ;; 数组整体赋值 → 拷贝循环
          (let [target-str (expr-str target-node context)
                val-str    (expr-str (n/assign-val node) context)
                size       (ty/vec-size target-type)
                loop-str   (emit-array-copy-loop target-str val-str (str size))]
            (make-fragment [loop-str] ""))
          ;; 标量赋值
          (let [var-str (expr-str target-node context)
                val-str (expr-str (n/assign-val node) context)]
            (make-fragment [(str var-str " = " val-str ";")] ""))))

      :else
      (fragment-expr (expr-str node context)))))

;; ── 发射 define 节点 ─────────────────────
(defmethod core/emit-node :define [node context]
  (let [type (ty/get-type node)]
    (cond
      (ty/fun-type? type)
      (let [lam        (or (n/define-val node)
                           (throw (ex-info "Define node has no value"
                                           {:node node :name (n/define-name node)})))
            fn-ty      (ty/get-type lam)
            ret-type   (if (ty/fun-type? fn-ty)
                         (core/hlsl-type-str (ty/fun-return-type fn-ty))
                         (throw (ex-info "Cannot determine return type"
                                         {:fn-name (n/define-name node)})))
            params     (n/lambda-params lam)
            param-vecs (mapv (fn [p]
                               [(core/hlsl-type-str (ty/get-type p))
                                (cname (n/var-name p))])
                             params)
            body-frag  (let [f (node->fragment (n/lambda-body lam) context)]
                         (if (str/blank? (:expr f))
                           f
                           (make-fragment (:stmts f) (str "return " (:expr f) ";"))))
            body-str   (str "{\n"
                            (->> (concat (:stmts body-frag)
                                         (when-not (str/blank? (:expr body-frag))
                                           [(:expr body-frag)]))
                                 (map #(str "  " %))
                                 (str/join "\n"))
                            "\n}")]
        [:function ret-type (cname (n/define-name node)) param-vecs body-str])

      :else
      (let [val (n/define-val node)
            val-kind (n/kind val)]
        (if (= val-kind :new-array)
          ;; 通过 new-array 直接初始化的数组
          (let [elem-type (core/hlsl-type-str (ty/vec-element-type type))
                name-str (cname (n/define-name node))
                size-str (expr-str (n/new-array-size val) context)]
            [:raw (str elem-type " " name-str "[" size-str "];")])
          ;; 其他初始化表达式
          (if (ty/vec-type? type)
            ;; ★ 数组类型但非 new-array：用 node->fragment 递归处理初始化表达式
            (let [size      (ty/vec-size type)
                  elem-type (core/hlsl-type-str (ty/vec-element-type type))
                  name-str  (cname (n/define-name node))
                  ;; 递归处理 val，得到片段（可能包含 let、block 等前置语句）
                  val-frag  (node->fragment val context)
                  val-str   (:expr val-frag)
                  pre-stmts (:stmts val-frag)
                  copy-loop (emit-array-copy-loop name-str val-str (str size))
                  ;; 拼接：前置语句 + 数组声明 + 拷贝循环
                  body-str  (str/join "\n" (concat (vec pre-stmts)
                                                   [(str elem-type " " name-str "[" size "];")
                                                    copy-loop]))]
              [:raw body-str])
            ;; 标量/向量/矩阵等普通变量
            (let [type-str (core/hlsl-type-str (ty/get-type val))
                  name-str (cname (n/define-name node))
                  frag     (node->fragment val context)
                  stmts    (conj (vec (:stmts frag))
                                 (str type-str " " name-str " = " (:expr frag) ";"))]
              [:raw (str/join "\n" stmts)])))))))