(ns run-tests
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :as t]))

(defn find-test-namespaces []
  (let [test-dir (io/file "test")
        files (file-seq test-dir)]
    (keep (fn [f]
            (when (and (.isFile f)
                       (.endsWith (.getName f) "_test.clj"))
              (let [path (.getPath f)
                    ;; 去掉 "test/" 前缀和 ".clj" 后缀
                    ns-path (subs path 5 (- (count path) 4))
                    ns-sym  (symbol (str/replace ns-path "/" "."))]
                ns-sym)))
          files)))

(doseq [ns-sym (find-test-namespaces)]
  (try
    (require ns-sym)
    (catch Exception e
      (println "Failed to require" ns-sym ":" (.getMessage e)))))
(let [results (apply t/run-tests (find-test-namespaces))]
  (when (pos? (+ (:error results 0) (:fail results 0)))
    (System/exit 1)))