(ns top.kzre.homunculus.core.ir1.preprocess
  "预处理表单"
  (:require [clojure.walk :as walk]
            [top.kzre.homunculus.core.ir1.expand-symbols :as ex]))


(defonce special-forms
         '#{ns fn let loop recur quote var set! try catch throw
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

;; ── 将 Clojure 内部特殊形式转换为编译器可识别的形式 ──
(defn- normalize-special-forms [form]
  (walk/prewalk
    (fn [x]
      (if (and (seq? x) (symbol? (first x)))
        (let [op (first x)]
          (case op
            fn*  (cons 'fn (rest x))
            let* (cons 'let (rest x))
            ;; 可继续添加其他转换
            x))
        x))
    form))

;; ── 将任何被错误限定的特殊形式恢复为短名 ──
(defn- fix-namespaced-special-forms [form]
  (walk/postwalk
    (fn [x]
      (if (symbol? x)
        (let [ns (namespace x)
              n  (name x)]
          (if (and ns (contains? special-forms (symbol n)))
            (symbol n)   ; 去掉命名空间，只保留短名
            x))
        x))
    form))

(defn preprocess [forms]
  (let [ns-form (first (filter ns-form? forms))
        _ (when-not ns-form
            (throw (ex-info "ns form is required" {})))
        ns-info (ex/resolve-ns ns-form)
        expanded (mapv #(try-expand-macro % ns-info) forms)
        normalized (mapv normalize-special-forms expanded)]
    (mapv fix-namespaced-special-forms normalized)))