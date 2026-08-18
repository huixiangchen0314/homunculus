(ns top.kzre.homunculus.backend.shader.metadata
  "着色器元数据访问辅助")

(defn shader-stage [node]
  (:shader/stage (:meta node)))

(defn shader-entry? [node]
  (:shader/entry? (:meta node)))

(defn shader-resource-kind [node]
  (:shader/resource-kind (:meta node)))

(defn shader-texture-register [node]
  (:shader/texture-register (:meta node)))

(defn shader-sampler-register [node]
  (:shader/sampler-register (:meta node)))

(defn shader-cbuffer-register [node]
  (:shader/cbuffer-register (:meta node)))

(defn shader-cbuffer-members [node]
  (:shader/cbuffer-members (:meta node)))

(defn shader-uniform? [node]
  (:shader/uniform? (:meta node)))

(defn shader-static-var? [node]
  (:shader/static-var? (:meta node)))

(defn shader-ignore-emit? [node]
  (:shader/ignore-emit? (:meta node)))

(defn shader-semantic [node]
  (let [md (:meta node)]
    (cond
      ;; 常见顶点/片元语义
      (:position md) :position
      (:normal md) :normal
      (:tangent md) :tangent
      (:bitangent md) :bitangent

      ;; 纹理坐标系列（0-7）
      (:texcoord0 md) :texcoord0
      (:texcoord1 md) :texcoord1
      (:texcoord2 md) :texcoord2
      (:texcoord3 md) :texcoord3
      (:texcoord4 md) :texcoord4
      (:texcoord5 md) :texcoord5
      (:texcoord6 md) :texcoord6
      (:texcoord7 md) :texcoord7

      ;; 颜色系列
      (:color0 md) :color0
      (:color1 md) :color1

      ;; 渲染目标输出（片元）
      (:target0 md) :target0
      (:target1 md) :target1
      (:depth md) :depth

      ;; 系统值
      (:instance-id md) :instance-id
      (:vertex-id md) :vertex-id
      (:primitive-id md) :primitive-id

      ;; 用户自定义属性
      (:user0 md) :user0
      (:user1 md) :user1
      (:user2 md) :user2
      (:user3 md) :user3

      :else nil)))