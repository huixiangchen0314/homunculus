(ns run-tests
  (:require [clojure.test :as t]
            [clojure.java.io :as io]))

(defn find-test-nses []
  (let [test-dir (io/file "test")
        files (file-seq test-dir)]
    (keep (fn [f]
            (when (and (.isFile f)
                       (.endsWith (.getName f) "_test.clj"))
              (let [path (.getPath f)
                    ;; 去掉 "test/" 前缀和 ".clj" 后缀
                    ns-path (subs path 5 (- (count path) 5))
                    ns-sym  (symbol (clojure.string/replace ns-path "/" "."))]
                ns-sym)))
          files)))

(defn -main []
  (doseq [ns-sym (find-test-nses)]
    (try
      (require ns-sym)
      (catch Exception e
        (println "Failed to require" ns-sym ":" (.getMessage e)))))
  (let [results (apply t/run-tests (find-test-nses))]
    (when (pos? (+ (:error results 0) (:fail results 0)))
      (System/exit 1))))