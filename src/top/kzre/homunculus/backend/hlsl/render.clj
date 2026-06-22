;; ── render.clj ───────────────────────────────────────────
(ns top.kzre.homunculus.backend.hlsl.render
  "HLSL 结构体 -> 字符串渲染器。
   缩进由 render-body / indent-str 统一管理，
   所有控制流体强制包裹花括号，aset 作为语句自带分号。"
  (:require [clojure.string :as str]
            [top.kzre.homunculus.backend.util.naming :refer [cname]]
            [top.kzre.homunculus.backend.hlsl.spec :as spec]))

(declare render-node)

(defn- indent-str [level]
  (apply str (repeat (* 4 level) \space)))

(defn- render-seq
  ([nodes] (render-seq nodes ", " 0))
  ([nodes separator] (render-seq nodes separator 0))
  ([nodes separator indent-level]
   (str/join separator (map #(render-node % indent-level) nodes))))

(defn- render-body
  [stmts indent-level]
  (str/join "\n" (map #(str (indent-str indent-level)
                            (render-node % indent-level))
                      stmts)))

(defn- render-param [[type name semantic] _]
  (str type " " (cname name) (when semantic (str " : " semantic))))

(defn- render-struct-member [[type name semantic] _]
  (str type " " (cname name) (when semantic (str " : " semantic)) ";"))

(defn- wrap-braces
  [stmts indent-level]
  (let [indent      (indent-str indent-level)
        inner-level (inc indent-level)
        inner-str   (render-body stmts inner-level)]
    (str indent "{\n"
         inner-str "\n"
         indent "}")))

(defn render-node [node indent-level]
  (cond
    (string? node) node
    (nil? node) ""
    (and (vector? node) (not (keyword? (first node))))
    (render-body node indent-level)
    :else
    (let [indent (indent-str indent-level)]
      (case (first node)
        :raw       (second node)
        :new-array (let [[_ size] node]
                     (str "[" (render-node size indent-level) "]"))
        :aget      (let [[_ target idx] node]
                     (str (render-node target indent-level) "[" (render-node idx indent-level) "]"))
        :aset      (let [[_ target idx val] node]
                     (str (render-node target indent-level)
                          "[" (render-node idx indent-level) "] = "
                          (render-node val indent-level)))
        :alength   (let [[_ target] node]
                     (str (render-node target indent-level) ".length"))
        :literal   (let [v (second node)]
                     (cond (nil? v) "" :else (str v)))
        :var-ref   (cname (second node))
        :call      (let [[_ fn-name & args] node]
                     (str (cname fn-name) "(" (render-seq args) ")"))
        :sample    (let [[_ tex sampler uv] node]
                     (str (render-node tex indent-level) ".Sample("
                          (render-node sampler indent-level) ", "
                          (render-node uv indent-level) ")"))
        :binary    (let [[_ op left right] node]
                     (if (and (= op "++") (= right [:literal 1]))
                       (str "++" (render-node left indent-level))
                       (str (render-node left indent-level) " " op " " (render-node right indent-level))))
        :member-access (let [[_ target member] node]
                         (str (render-node target indent-level) "." (cname member)))
        :constructor (let [[_ type-name & args] node]
                       (str type-name "(" (render-seq args) ")"))
        :cast      (let [[_ type-str expr] node]
                     (str "(" type-str ")" (render-node expr indent-level)))

        :return    (str "return " (render-node (second node) indent-level))
        :var-decl  (let [[_ type name init] node]
                     (str type " " (cname name) " = " (render-node init indent-level)))
        :array-decl (let [[_ elem-type name size] node]
                      (str elem-type " " (cname name) "[" (render-node size indent-level) "]"))
        :assign    (let [[_ target expr] node]
                     (str (render-node target indent-level) " = " (render-node expr indent-level)))
        :expr-stmt (let [[_ expr] node]
                     (str (render-node expr indent-level) ";"))

        ;; while：强制花括号
        :while     (let [[_ test body] node]
                     (str "while (" (render-node test indent-level) ")\n"
                          (wrap-braces (if (and (vector? body) (not (keyword? (first body))))
                                         body
                                         [body])
                                       indent-level)))

        ;; if：强制花括号，else 对齐
        :if        (let [[_ test then else] node]
                     (str "if (" (render-node test indent-level) ")\n"
                          (wrap-braces (if (and (vector? then) (not (keyword? (first then))))
                                         then
                                         [then])
                                       indent-level)
                          (when else
                            (str "\n" indent "else\n"
                                 (wrap-braces (if (and (vector? else) (not (keyword? (first else))))
                                                else
                                                [else])
                                              indent-level)))))

        :function  (let [[_ return-type name params body] node]
                     (str return-type " " (cname name) "("
                          (str/join ", " (map #(render-param % indent-level) params))
                          ")\n"
                          (if (string? body)
                            body
                            (wrap-braces (if (and (vector? body) (not (keyword? (first body))))
                                           body
                                           [body])
                                         indent-level))))

        :entry-wrapper (let [[_ stage func-name input-type output-type return-type body] node]
                         (case stage
                           :vertex   (str output-type " " (cname func-name) "(" input-type " input)\n"
                                          (if (string? body)
                                            body
                                            (wrap-braces (if (and (vector? body) (not (keyword? (first body))))
                                                           body
                                                           [body])
                                                         indent-level)))
                           :fragment (str return-type " " (cname func-name) "(" input-type " input) : SV_TARGET\n"
                                          (if (string? body)
                                            body
                                            (wrap-braces (if (and (vector? body) (not (keyword? (first body))))
                                                           body
                                                           [body])
                                                         indent-level)))))

        :import    (let [[_ path] node]
                     (str "#include \"" path "\""))
        :struct-member (render-struct-member (rest node) indent-level)
        :struct    (let [[_ name members] node]
                     (str "struct " (cname name) " {\n"
                          (render-body members (inc indent-level))
                          "\n" indent "};"))
        :texture-decl   (let [[_ name reg] node] (str "Texture2D " (cname name) " : register(" reg ");"))
        :sampler-decl   (let [[_ name reg] node] (str "SamplerState " (cname name) " : register(" reg ");"))
        :cbuffer-decl   (let [[_ name reg members] node]
                          (str "cbuffer " (cname name) " : register(" reg ") {\n"
                               (render-body members (inc indent-level))
                               "\n" indent "};"))
        :uniform-decl   (let [[_ type name] node] (str "uniform " type " " (cname name) ";"))
        :static-var-decl (let [[_ type name init] node]
                           (str "static " type " " (cname name) " = " (render-node init indent-level) ";"))
        :comment    (str "// " (second node))

        (throw (ex-info (str "Unknown HLSL structure tag: " (first node)) {:node node}))))))

(defn render
  [nodes]
  (when-not (spec/valid-ast? nodes)
    (throw (ex-info (str "Invalid HLSL AST" (spec/explain-ast nodes))
                    {:explain (spec/explain-ast nodes)})))
  (str/join "\n\n" (map #(render-node % 0) nodes)))