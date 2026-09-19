(ns top.kzre.homunculus.core.ir2.pass.module.api
  (:require [top.kzre.homunculus.core.ir2.pass.module.collect-symbols :as cs]
            [top.kzre.homunculus.core.ir2.pass.module.resolve-ns :as rn]))

(def resolve-ns rn/resolve-ns)
(def collect-symbols cs/collect-symbols)


