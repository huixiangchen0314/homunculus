(ns top.kzre.homunculus.core.ir1.preprocess
  "预处理表单"
  (:require [top.kzre.homunculus.core.ir1.expand-symbols :as ex]))


(defonce special-forms
         '#{ns fn* let loop* recur quote var set! try catch throw
            if do . def defrecord defprotocol})

(defn ns-form?  "判断一个形式是否是 ns."
  [f] (and (seq? f) (= 'ns (first f))))

(defn- try-expand-macro [form ns-info]
  (loop [f form, limit 10]
    (if (and (seq? f) (symbol? (first f)))
      (let [op (first f)]
        (if (special-forms op)
          f
          (if (zero? limit)
            (throw (ex-info "Macro expansion depth exceeded" {:form f}))
            (let [qualified-op (try (ex/expand-sym op ns-info)
                                    (catch Exception _ op))
                  qualified-form (if (= qualified-op op)
                                   f
                                   (cons qualified-op (rest f)))
                  macro-var    (try (resolve qualified-op)
                                    (catch Exception _ nil))]
              (if (and macro-var (:macro (meta macro-var)))
                (let [expanded (macroexpand-1 qualified-form)]
                  (if (= expanded qualified-form)
                    f  ;; 展开无变化，退出
                    (recur expanded (dec limit))))
                f)))))
      f)))


(defn preprocess [forms]
  (let [ns-form (first (filter ns-form? forms))
        _ (when-not ns-form
            (throw (ex-info "ns form is required" {})))
        ns-info (ex/resolve-ns ns-form)]
    (mapv #(try-expand-macro % ns-info) forms)))
