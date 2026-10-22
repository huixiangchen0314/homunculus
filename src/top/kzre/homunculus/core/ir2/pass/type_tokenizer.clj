(ns top.kzre.homunculus.core.ir2.pass.type-tokenizer
  "Java TypeTokenizer 的 Clojure 包装——把 Java enum 转 keyword，
   把 Token 转 Clojure map，便于 parse-type-symbol 消费。"
  (:import
    [top.kzre.homunculus.core.ir2.pass TypeTokenizer
                                       TypeTokenizer$TokenType TypeTokenizer$Token]))

(def ^:private token-type->kw
  {TypeTokenizer$TokenType/IDENT    :ident
   TypeTokenizer$TokenType/TYPE_VAR :type-var
   TypeTokenizer$TokenType/INT      :int
   TypeTokenizer$TokenType/ARROW    :arrow
   TypeTokenizer$TokenType/LBRACKET :lbracket
   TypeTokenizer$TokenType/RBRACKET :rbracket
   TypeTokenizer$TokenType/LANGLE   :langle
   TypeTokenizer$TokenType/RANGLE   :rangle
   TypeTokenizer$TokenType/LPAREN   :lparen
   TypeTokenizer$TokenType/RPAREN   :rparen
   TypeTokenizer$TokenType/COMMA    :comma
   TypeTokenizer$TokenType/EOF      :eof})

(defn tokenize
  "把类型字符串切分为 [{:type :ident :text \"float4\" :pos 0} ...]。
   末尾总有 {:type :eof}。"
  [^String src]
  (mapv (fn [^TypeTokenizer$Token t]
          {:type (token-type->kw (.type t))
           :text (.text t)
           :pos  (.position t)})
        (TypeTokenizer/tokenize src)))