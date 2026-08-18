(ns top.kzre.homunculus.core.types.io-propagate
  "副作用标记传播pass"
  (:require [top.kzre.homunculus.internal.protocol :as proto]
            [top.kzre.homunculus.internal.symbol :as sym]))

(defn- fn-fx?
  "查询函数是否有副作用。默认不纯（即未标记 pure? 的视为有副作用）。"
  [fn-name ctx]
  (let [tbl (proto/symbol-table ctx)
        entry (sym/lookup-sym tbl fn-name)]
    (sym/func-fx? entry)))