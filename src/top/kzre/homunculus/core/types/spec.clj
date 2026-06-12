(ns top.kzre.homunculus.core.types.spec
  "IR2 节点的 attrs 字段规约文档与辅助访问函数。")

;; ── 键名常量 ──────────────────────────
(def k-type       :type)        ;; IType 实例，节点推导类型
(def k-mutable    :mutable)     ;; boolean，变量是否可变
(def k-builtin-fn :builtin-fn)  ;; TFun，内建函数类型签名
(def k-shader-stage :shader-stage) ;; keyword，:vertex, :fragment 等
(def k-semantic   :semantic)    ;; string，HLSL 语义如 "SV_Position"
(def k-resource   :resource)    ;; map，资源信息 {:kind :texture2D, :binding 0}
(def k-ctor       :ctor)        ;; boolean 或 map，标记为构造函数
(def k-convert    :convert)     ;; map，类型转换信息 {:src-type .. :dst-type ..}

;; ── 访问辅助 ──────────────────────────
(defn get-type [node] (get-in node [:attrs k-type]))
(defn mutable? [node] (get-in node [:attrs k-mutable]))
(defn builtin-fn [node] (get-in node [:attrs k-builtin-fn]))
(defn shader-stage [node] (get-in node [:attrs k-shader-stage]))
(defn semantic [node] (get-in node [:attrs k-semantic]))
(defn resource-info [node] (get-in node [:attrs k-resource]))