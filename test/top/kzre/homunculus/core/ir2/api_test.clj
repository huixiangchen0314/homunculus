(ns top.kzre.homunculus.core.ir2.api-test
  (:require
   [clojure.test :refer [deftest is]]
   [top.kzre.homunculus.core.ir2.api :as subject]))

(deftest ->ir2-test
  (is (= true
         (subject/foo))))