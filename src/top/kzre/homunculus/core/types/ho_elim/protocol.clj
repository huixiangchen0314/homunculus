(ns top.kzre.homunculus.core.types.ho-elim.protocol)

(defprotocol IHoElimConfig
  (known-ho-functions [this]
    "返回一个 map，键为函数符号（如 'map, 'reduce），值为转换策略关键字，如 :map, :reduce, :filter 等。")
  (supports-dynamic-collections? [this]
    "返回 true 表示后端支持对动态长度集合进行高阶函数展开（生成循环）。")
  (backend-length-fn [this]
    "返回用于获取集合长度的内置函数名（符号），如 'count。")
  (backend-nth-fn [this]
    "返回用于随机访问集合元素的内置函数名（符号），如 'nth。")
  (backend-less-than-fn [this]
    "返回用于比较索引的二元运算符函数名（符号），如 '<。"))