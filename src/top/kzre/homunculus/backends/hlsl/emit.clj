(ns top.kzre.homunculus.backends.hlsl.emit
  (:require [top.kzre.homunculus.backends.hlsl.ir3-hlsl :as ir3]
            [clojure.string :as str])
  (:import (top.kzre.homunculus.backends.hlsl.ir3_hlsl
             HlslLiteral HlslVarRef HlslBinaryOp HlslCall
             HlslConstructor HlslSwizzle HlslMemberAccess
             HlslIfExpr HlslLetExpr
             HlslExprStmt HlslIfStmt HlslReturnStmt
             HlslLoopStmt HlslVarDecl HlslFunction HlslProgram)))

(def type-names ir3/hlsl-types)

(defn- indent [level]
  (apply str (repeat (* level 2) \space)))

(defn emit-expr [expr]
  (cond
    (instance? HlslLiteral expr)
    (let [v (:value expr)]
      (cond
        (number? v) (str v)
        (true? v) "true"
        (false? v) "false"
        (nil? v) "false"
        :else (str v)))

    (instance? HlslVarRef expr)
    (name (:name expr))

    (instance? HlslBinaryOp expr)
    (let [op (:op expr)
          left (emit-expr (:left expr))
          right (when (:right expr) (emit-expr (:right expr)))]
      (case op
        :! (str "!" left)
        (str "(" left " " (name op) " " right ")")))

    (instance? HlslCall expr)
    (let [func (:func expr)
          args (map emit-expr (:args expr))]
      (str func "(" (str/join ", " args) ")"))

    (instance? HlslConstructor expr)
    (let [base-type (:base-type expr)
          elems (map emit-expr (:elements expr))
          dim (count elems)
          type-str (type-names base-type base-type)]
      (if (contains? #{:float :int :bool} base-type)
        ;; 标量基础类型 → 根据维度生成 float2/int3 等
        (if (= dim 1)
          (str "(" type-str ") " (first elems))
          (str type-str dim "(" (str/join ", " elems) ")"))
        ;; 已经是向量/矩阵类型 → 直接使用该类型名
        (str type-str "(" (str/join ", " elems) ")")))

    (instance? HlslSwizzle expr)
    (let [base (emit-expr (:base expr))
          swizzle (:swizzle expr)]
      (str base "." swizzle))

    (instance? HlslMemberAccess expr)
    (let [base (emit-expr (:base expr))
          member (:member expr)]
      (str base "." (if (string? member) member (name member))))

    (instance? HlslIfExpr expr)
    (let [test (emit-expr (:test expr))
          then (emit-expr (:then expr))
          else (when (:else expr) (emit-expr (:else expr)))]
      (if else
        (str "(" test " ? " then " : " else ")")
        (str "(" test " ? " then " : false)")))

    (instance? HlslLetExpr expr)
    "/* let expression not supported in HLSL */"

    :else
    (str "<unknown expr>")))

(defn emit-stmt
  ([stmt] (emit-stmt stmt 0))
  ([stmt level]
   (cond
     (instance? HlslExprStmt stmt)
     (str (indent level) (emit-expr (:expr stmt)) ";\n")

     (instance? HlslVarDecl stmt)
     (let [name (:name stmt)
           type (type-names (:type stmt) "float")
           init (when (:init stmt) (emit-expr (:init stmt)))]
       (if init
         (str (indent level) type " " name " = " init ";\n")
         (str (indent level) type " " name ";\n")))

     (instance? HlslIfStmt stmt)
     (let [test (emit-expr (:test stmt))
           then-stmts (map #(emit-stmt % (inc level)) (:then stmt))
           else-stmts (when (seq (:else stmt))
                        (map #(emit-stmt % (inc level)) (:else stmt)))]
       (str (indent level) "if (" test ")\n"
            (indent level) "{\n"
            (apply str then-stmts)
            (indent level) "}\n"
            (when else-stmts
              (str (indent level) "else\n"
                   (indent level) "{\n"
                   (apply str else-stmts)
                   (indent level) "}\n"))))

     (instance? HlslReturnStmt stmt)
     (str (indent level) "return " (emit-expr (:expr stmt)) ";\n")

     (instance? HlslLoopStmt stmt)
     (let [bindings (:bindings stmt)
           body (:body stmt)
           local-decls (map (fn [[n init]]
                              (str (indent level) "float " (name n) " = " (emit-expr init) ";\n"))
                            bindings)
           body-stmts (map #(emit-stmt % (inc level)) body)]
       (str (apply str local-decls)
            (indent level) "for(;;)\n"
            (indent level) "{\n"
            (apply str body-stmts)
            (indent level) "}\n"))

     (instance? HlslFunction stmt)
     (let [name (:name stmt)
           params (:params stmt)
           return-type (type-names (:return-type stmt) "void")
           body (:body stmt)
           param-str (->> params
                          (map (fn [p]
                                 (if (string? p)
                                   p   ;; 已经是 "float3 normal" 形式
                                   (str "float " (name p)))))
                          (str/join ", "))
           body-stmts (map #(emit-stmt % (inc level)) body)]
       (str return-type " " name "(" param-str ")\n"
            (indent level) "{\n"
            (apply str body-stmts)
            (indent level) "}\n"))

     :else "")))

(defn emit-program [prog]
  (let [uniforms (:uniforms prog)
        functions (:functions prog)]
    (str
      (apply str (map #(emit-stmt %) uniforms))
      "\n"
      (apply str (map #(emit-stmt %) functions)))))