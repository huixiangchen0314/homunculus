(ns top.kzre.homunculus.core.ir1.preprocess
  "预处理表单，保留所有表单的元数据（包括源码位置）。"
  (:require [top.kzre.homunculus.internal.walk :as walk]
            [top.kzre.homunculus.core.ir1.expand-symbols :as ex]))

(defonce special-forms
         '#{ns fn let loop recur quote var set! try catch throw
            if do . def defrecord defprotocol})

(defn ns-form? [f] (and (seq? f) (= 'ns (first f))))

(defn- try-expand-macro [form ns-info]
  (let [orig-meta (meta form)]
    (loop [f form, limit 10]
      (if (and (seq? f) (symbol? (first f)))
        (let [op (first f)]
          (if (special-forms op)
            (vary-meta f merge orig-meta)  ;; 保留原始meta
            (if (zero? limit)
              (throw (ex-info "Macro expansion depth exceeded" {:form f}))
              (let [qualified-op (try (ex/expand-sym op ns-info)
                                      (catch Exception _ op))
                    qualified-form (if (= qualified-op op)
                                     f
                                     (with-meta (cons qualified-op (rest f)) (meta f)))
                    macro-var (try (resolve qualified-op)
                                   (catch Exception _ nil))]
                (if (and macro-var (:macro (meta macro-var)))
                  (let [expanded (macroexpand-1 qualified-form)]
                    (if (= expanded qualified-form)
                      (vary-meta f merge orig-meta)
                      (recur expanded (dec limit))))
                  (vary-meta f merge orig-meta))))))
        (vary-meta f merge orig-meta)))))

(defn- normalize-special-forms [form]
  (walk/prewalk
    (fn [x]
      (if (and (seq? x) (symbol? (first x)))
        (let [op (first x)
              m  (meta x)]
          (case op
            fn*  (with-meta (cons 'fn (rest x)) m)
            let* (with-meta (cons 'let (rest x)) m)
            x))
        x))
    form))

(defn- fix-namespaced-special-forms [form]
  (walk/postwalk
    (fn [x]
      (if (symbol? x)
        (let [ns (namespace x)
              n  (name x)]
          (if (and ns (contains? special-forms (symbol n)))
            (with-meta (symbol n) (meta x))   ;; 复制元数据
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