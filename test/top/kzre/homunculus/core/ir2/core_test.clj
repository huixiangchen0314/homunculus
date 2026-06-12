
(ns top.kzre.homunculus.core.ir2.core-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.forms]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as p]))

(defn- node? [node expected-kind]
  (and (satisfies? p/INode node)
       (= expected-kind (p/kind node))))

(defn- literal-node? [node val]
  (and (node? node :literal) (= val (:val node))))

(defn- variable-node? [node name]
  (and (node? node :variable) (= name (:name node))))

(defn- call-node? [node] (node? node :call))

(deftest literal-lowering-test
  (let [roots (ir2/lower [(ir1/->ir1 42)])
        node  (first roots)]
    (is (literal-node? node 42))
    (is (nil? (p/attrs node)))
    (is (empty? (p/children node)))))
