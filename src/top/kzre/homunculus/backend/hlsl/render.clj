(ns top.kzre.homunculus.backend.hlsl.render
  "HLSL 结构体 -> 字符串渲染器。
   处理顶层结构体，函数体字符串原样输出。
   集成了 hlsl.spec 对输入数据进行校验。"
  (:require [clojure.string :as str]
            [top.kzre.homunculus.backend.util.naming :refer [cname]]
            [top.kzre.homunculus.backend.hlsl.spec :as spec]))

(declare render-node)

(defn- render-seq
  ([nodes] (render-seq nodes ", " 0))
  ([nodes separator] (render-seq nodes separator 0))
  ([nodes separator indent-level]
   (str/join separator (map #(render-node % indent-level) nodes))))

(defn- render-body
  [stmts indent-level]
  (str/join "\n" (map #(let [r (render-node % indent-level)]
                         (str (apply str (repeat (* 4 indent-level) \space)) r))
                      stmts)))

(defn- render-param [[type name semantic] _]
  (str type " " (cname name) (when semantic (str " : " semantic))))

(defn- render-struct-member [[type name semantic] _]
  (str type " " (cname name) (when semantic (str " : " semantic)) ";"))

(defn render-node [node indent-level]
  (cond
    (string? node) node
    (nil? node) ""
    :else
    (let [indent (apply str (repeat (* 4 indent-level) \space))]
      (case (first node)
        :raw       (second node)
        ;; 数组节点
        :new-array (let [[_ size] node] (str "[" size "]"))
        :aget      (let [[_ target idx] node]
                     (str (render-node target indent-level) "[" (render-node idx indent-level) "]"))
        :aset      (let [[_ target idx val] node]
                     (str (render-node target indent-level)
                          "[" (render-node idx indent-level) "] = "
                          (render-node val indent-level)))
        :alength   (let [[_ target] node]
                     (str (render-node target indent-level) ".length"))
        :literal   (str (second node))
        :var-ref   (cname (second node))
        :call      (let [[_ fn-name & args] node]
                     (str (cname fn-name) "(" (render-seq args) ")"))
        :sample    (let [[_ tex sampler uv] node]
                     (str (render-node tex indent-level) ".Sample("
                          (render-node sampler indent-level) ", "
                          (render-node uv indent-level) ")"))
        :binary    (let [[_ op left right] node]
                     (str (render-node left indent-level) " " op " " (render-node right indent-level)))
        :member-access (let [[_ target member] node]
                         (str (render-node target indent-level) "." (cname member)))
        :constructor (let [[_ type-name & args] node]
                       (str type-name "(" (render-seq args) ")"))
        :cast      (let [[_ type-str expr] node]
                     (str "(" type-str ")" (render-node expr indent-level)))

        :return    (str "return " (render-node (second node) indent-level) ";")
        :var-decl  (let [[_ type name init] node]
                     (str type " " (cname name) " = " (render-node init indent-level) ";"))
        :assign    (let [[_ target expr] node]
                     (str (render-node target indent-level) " = " (render-node expr indent-level) ";"))
        :expr-stmt (let [[_ expr] node]
                     (str (render-node expr indent-level) ";"))

        :if        (let [[_ test then else] node]
                     (str "if (" (render-node test indent-level) ")\n"
                          (render-node then indent-level)
                          (when else (str "\nelse\n" (render-node else indent-level)))))
        :while     (let [[_ test body] node]
                     (str "while (" (render-node test indent-level) ")\n"
                          (render-node body indent-level)))

        :block     (let [[_ & stmts] node]
                     (str "{\n"
                          (render-body stmts (inc indent-level))
                          "\n" indent "}"))
        :function  (let [[_ return-type name params body] node]
                     (str return-type " " (cname name) "("
                          (str/join ", " (map #(render-param % indent-level) params))
                          ")\n"
                          (if (string? body)
                            body
                            (render-body body (inc indent-level)))))
        :entry-wrapper (let [[_ stage func-name input-type output-type return-type body] node]
                         (case stage
                           :vertex   (str output-type " " (cname func-name) "(" input-type " input)\n{\n"
                                          (if (string? body)
                                            body
                                            (render-body body (inc indent-level)))
                                          "\n}")
                           :fragment (str return-type " " (cname func-name) "(" input-type " input) : SV_TARGET\n{\n"
                                          (if (string? body)
                                            body
                                            (render-body body (inc indent-level)))
                                          "\n}")))
        ;; 导入指令
        :import    (let [[_ path] node]
                     (str "#include \"" path "\""))
        ;; 结构体成员（用于 struct 和 cbuffer 内部）
        :struct-member (render-struct-member (rest node) indent-level)
        ;; 结构体定义
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
  "渲染 HLSL AST 节点列表为字符串。若 nodes 不符合 spec，抛出异常。"
  [nodes]
  (when-not (spec/valid-ast? nodes)
    (throw (ex-info (str "Invalid HLSL AST" (spec/explain-ast nodes))
                    {:explain (spec/explain-ast nodes)})))
  (str/join "\n\n" (map #(render-node % 0) nodes)))