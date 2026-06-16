(ns top.kzre.homunculus.core.types.ho-elim.protocol)

(defprotocol IHoElimConfig
  (known-ho-functions [this])
  (supports-variable-collections? [this]
    "返回 true 表示后端支持对变长集合进行高阶函数展开（生成循环）。")
  (backend-length-fn [this])
  (backend-nth-fn [this])
  (backend-less-than-fn [this])
  ;; 变长集合操作
  (backend-variable-collection-ctor [this length element-type]
    "返回一个 IR2 节点，表示创建一个长度为 length、元素类型为 element-type 的变长集合。")
  (backend-variable-collection-set [this collection-node index-node value-node]
    "返回一个 IR2 节点，表示将变长集合 collection-node 中索引 index-node 处的值设置为 value-node。")
  (backend-add-fn [this]
    "返回后端用于加法的函数符号，如 '+ 或 'add。"))