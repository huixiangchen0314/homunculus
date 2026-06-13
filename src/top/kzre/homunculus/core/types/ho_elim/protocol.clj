(ns top.kzre.homunculus.core.types.ho-elim.protocol)

(defprotocol IHoElimConfig
  (known-ho-functions [this]
    "返回一个 map，键为函数符号（如 'map, 'reduce），值为转换策略关键字，如 :map, :reduce, :filter 等。"))