(ns top.kzre.homunculus.backend.glsl.sematic.location
  "默认的语义布局。

   GLSL 的 location 按空间独立：
   ▸ vertex-input    —— 顶点属性槽位
   ▸ varying         —— vertex out / fragment in 共享的槽位
   ▸ fragment-output —— render target 槽位

   三个空间各有一张默认表。同一 semantic 在不同空间里占不同槽位——
   :normal 在 vertex-input 里是 1，在 varying 里是 0。

   ── 分配原则 ──────────────────────────────────────────
   追加式——高频 semantic 优先占低位槽位，新 semantic 顺延。不追求
   各空间之间槽位编号一致——三个空间本来就是独立的硬件资源。

   vertex-input 封顶 16——GL_MAX_VERTEX_ATTRIBS 的最低保证是 16，
   user2 / user3 超出最低保证，默认不分配，用户需要时显式配置
   或使用 :auto 按声明顺序分配。

   varying 和 fragment-output 空间宽裕——user2 / user3 进表；
   fragment-output 覆盖 target0-7（GL_MAX_DRAW_BUFFERS 的最低
   保证是 8）。

   ── 不该进表的 semantic ──────────────────────────────
   ▸ :position（varying 空间）—— vertex out 映射 gl_Position，
     fragment in 映射 gl_FragCoord，都是 builtin
   ▸ :depth —— 映射 gl_FragDepth，builtin
   ▸ :instance-id / :vertex-id / :primitive-id —— 映射 gl_InstanceID /
     gl_VertexID / gl_PrimitiveID，builtin

   这些 semantic 由 builtin-semantic? 识别，不走 location 分配。")

(def ^:private default-vertex-input-locations
  {:position  0
   :normal    1
   :texcoord0 2
   :tangent   3
   :bitangent 4
   :color0    5
   :color1    6
   :texcoord1 7
   :texcoord2 8
   :texcoord3 9
   :texcoord4 10
   :texcoord5 11
   :texcoord6 12
   :texcoord7 13
   :user0     14
   :user1     15})

(def ^:private default-varying-locations
  {:normal    0
   :texcoord0 1
   :tangent   2
   :bitangent 3
   :color0    4
   :color1    5
   :texcoord1 6
   :texcoord2 7
   :texcoord3 8
   :texcoord4 9
   :texcoord5 10
   :texcoord6 11
   :texcoord7 12
   :user0     13
   :user1     14
   :user2     15
   :user3     16})

(def ^:private default-fragment-output-locations
  {:target0 0
   :target1 1
   :target2 2
   :target3 3
   :target4 4
   :target5 5
   :target6 6
   :target7 7})

(defn default-vertex-input-location
  "vertex-input 空间的默认 location。未登记的 semantic 返回 nil。"
  [semantic]
  (get default-vertex-input-locations semantic))

(defn default-varying-location
  "varying 空间的默认 location。未登记的 semantic 返回 nil。"
  [semantic]
  (get default-varying-locations semantic))

(defn default-fragment-output-location
  "fragment-output 空间的默认 location。未登记的 semantic 返回 nil。

   单 fragColor 不走这张表——plan-fragment-return 的标量分支
   硬编码 location 0。多 render target 的 struct 返回字段标
   :target0 / :target1 等时，查这张表。"
  [semantic]
  (get default-fragment-output-locations semantic))