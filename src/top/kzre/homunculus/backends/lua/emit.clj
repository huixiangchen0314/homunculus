(ns top.kzre.homunculus.backends.lua.emit
  "解析IR2生成 Lua 代码"
  (:require [top.kzre.homunculus.core.ir2 :as ir2]
            [clojure.string :as str]))

(defn- indent [level]
  (apply str (repeat (* level 2) \space)))

(def ^:private prim-ops-lua
  {:add   "+"
   :sub   "-"
   :mul   "*"
   :div   "/"
   :mod   "%"
   :rem   "%"
   :eq    "=="
   :neq   "~="
   :lt    "<"
   :lte   "<="
   :gt    ">"
   :gte   ">="
   :and   "and"
   :or    "or"
   :not   nil
   :inc   nil
   :dec   nil
   :first nil
   :second nil
   :rest  nil
   :nth   nil
   :count nil
   :conj  nil
   :assoc nil
   :dissoc nil
   :get   nil
   :str   nil
   :print "print"})

(defn emit-expr
  "将 IR2 向量转换为 Lua 表达式字符串。"
  [ir2-vec]
  (let [node (first ir2-vec)
        kind (::ir2/kind node)]
    (case kind
      :literal
      (let [v (:val node)]
        (cond
          (number? v) (str v)
          (string? v) (pr-str v)
          (nil? v)    "nil"
          (true? v)   "true"
          (false? v)  "false"
          (keyword? v) (pr-str (name v))
          (char? v)   (str (int v))
          :else       (str v)))

      :var
      (name (:name node))

      :prim
      (let [op (:op node)
            args (map emit-expr (rest ir2-vec))]
        (case op
          :inc (str "(" (first args) " + 1)")
          :dec (str "(" (first args) " - 1)")
          :not (str "not " (first args))
          :print (str "print(" (str/join ", " args) ")")
          :first (str (first args) "[1]")
          :second (str (first args) "[2]")
          :nth (let [coll (first args) idx (second args)]
                 (str coll "[" idx " + 1]"))
          :count (str "#" (first args))
          :get (str (first args) "[" (emit-expr (second (rest ir2-vec))) "]")
          :str (str "tostring(" (first args) ")")
          ;; 二元运算符
          (if-let [lua-op (prim-ops-lua op)]
            (if (#{:add :sub :mul :div :mod :rem :eq :neq :lt :lte :gt :gte :and :or} op)
              (str "(" (first args) " " lua-op " " (second args) ")")
              (str lua-op "(" (str/join ", " args) ")"))
            (str op "(" (str/join ", " args) ")"))))

      :call
      (let [func (emit-expr (second ir2-vec))
            args (map emit-expr (nthrest ir2-vec 2))]
        (str func "(" (str/join ", " args) ")"))

      :if
      (let [test (emit-expr (second ir2-vec))
            then (emit-expr (nth ir2-vec 2))
            else (when (>= (count ir2-vec) 4) (emit-expr (nth ir2-vec 3)))]
        (if else
          (str "(" test " and " then " or " else ")")
          (str "(" test " and " then " or nil)")))

      :do
      ;; 在表达式位置使用 IIFE
      (let [body-irs (rest ir2-vec)
            stmts (str/join " " (map emit-expr body-irs))]
        (str "(function() " stmts " end)()"))

      :let
      (let [bindings-count (:bindings-count node)
            all-children (rest ir2-vec)]
        (if bindings-count
          (let [bind-pairs (take bindings-count all-children)
                body-irs (drop bindings-count all-children)
                local-decls (str/join " "
                                      (map (fn [[sym val]]
                                             (str "local " (emit-expr sym) " = " (emit-expr val)))
                                           (partition 2 bind-pairs)))
                body-exprs (str/join " " (map emit-expr body-irs))]
            (str "(function() " local-decls " return " body-exprs " end)()"))
          (str "(function() return nil end)()")))

      :fn
      (let [params (::ir2/params node)
            fn-name (::ir2/fn-name node)
            body-irs (rest ir2-vec)
            body-stmts (str/join " " (map emit-expr body-irs))]
        (str "function(" (str/join ", " (map name params)) ") "
             body-stmts " end"))

      :loop
      "nil"

      (str "nil"))))

(defn emit-stmt
  "将 IR2 向量作为语句发射，返回带缩进的多行代码。"
  ([ir2-vec] (emit-stmt ir2-vec 0))
  ([ir2-vec level]
   (let [node (first ir2-vec)
         kind (::ir2/kind node)]
     (case kind
       ;; 简单表达式语句
       (:literal :var :prim :call :let :fn)
       (str (indent level) (emit-expr ir2-vec) "\n")

       :do
       (str/join (map #(emit-stmt % level) (rest ir2-vec)))

       :if
       (let [test (emit-expr (second ir2-vec))
             then (emit-stmt (nth ir2-vec 2) (inc level))
             else (when (>= (count ir2-vec) 4)
                    (emit-stmt (nth ir2-vec 3) (inc level)))]
         (str (indent level) "if " test " then\n"
              then
              (when else
                (str (indent level) "else\n" else))
              (indent level) "end\n"))

       :def
       (let [name (emit-expr (second ir2-vec))
             val (when (>= (count ir2-vec) 3) (emit-expr (nth ir2-vec 2)))]
         (str (indent level) name " = " val "\n"))

       :vector
       (let [elems (str/join ", " (map emit-expr (rest ir2-vec)))]
         (str (indent level) "{" elems "}\n"))

       :map
       (let [pairs (str/join ", "
                             (map (fn [[k v]]
                                    (str "[" (emit-expr k) "] = " (emit-expr v)))
                                  (partition 2 (rest ir2-vec))))]
         (str (indent level) "{" pairs "}\n"))

       :loop
       (let [bindings-count (:bindings-count node)
             all-children (rest ir2-vec)
             bind-pairs (take bindings-count all-children)
             body-irs (drop bindings-count all-children)
             syms (map first (partition 2 bind-pairs))
             vals (map second (partition 2 bind-pairs))]
         (str (indent level) "local " (str/join ", " (map emit-expr syms))
              " = " (str/join ", " (map emit-expr vals)) "\n"
              (indent level) "while true do\n"
              (str/join (map #(emit-stmt % (inc level)) body-irs))
              (indent level) "end\n"))

       :recur
       (str (indent level) "-- recur not fully implemented\n")

       ;; 其他
       (str (indent level) (emit-expr ir2-vec) "\n")))))

(defn ir2->lua
  "将 IR2 向量列表转换为完整的 Lua 源代码字符串。"
  [ir2-vecs]
  (str/join "\n" (map #(emit-stmt %) ir2-vecs)))