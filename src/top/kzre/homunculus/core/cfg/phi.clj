(ns top.kzre.homunculus.core.cfg.phi
  "phi 节点——SSA 的通用数据结构。

   {:var-name sym :origin stmt-or-var :inputs [Input ...]}

   Phi 的 :var-name —— phi 定义的变量名（base name）。construct 的
                      rename 阶段用 base name 而非版本号——destruct
                      下推时 input 的 var-name 被替换成 phi 的
                      var-name，版本化自然消除。

   Phi 的 :origin —— 定义该 phi var 的原始 stmt 或 variable 节点，
                     用于 SSA 重命名和 phi 消除时构造新节点（带 attrs）。
                     由 collect-defs 收集，phi 构造时传入。
                     约定非 nil——nil 在 make-var-ref/make-move 处抛异常。

   Phi 的 :inputs —— 各前驱的完整输入。phi 插入阶段为空——由
                     construct 的 rename 阶段通过 update-phi-input
                     逐个填充。每个 Input 的 predecessor-id / origin /
                     var-name 都非 nil。

   Input 的 :predecessor-id —— 前驱块 id。
   Input 的 :origin —— 该前驱块里 phi-var 对应的值节点（variable ref）。
   Input 的 :var-name —— 该前驱块里 phi-var 对应的原变量名（带 SSA 版本号）。

   ── 构造约定 ──

   所有构造都要求完整元信息——不允许残缺 Input。
   make-phi 的 inputs 初始为空——不构造占位 Input。
   update-phi-input 是 upsert——已有则更新，无则添加。")

(defrecord Input [predecessor-id origin var-name])

(defn make-input
  "构造完整的 Input。

   predecessor-id —— 前驱块 id
   origin         —— 该前驱传入的值节点（variable ref）
   var-name       —— 该前驱传入的变量名（带 SSA 版本号）"
  [predecessor-id origin var-name]
  (->Input predecessor-id origin var-name))

(defrecord Phi [var-name origin inputs])

(defn make-phi
  "构造 phi。inputs 初始为空——由 construct 的 rename 阶段通过
   update-phi-input 逐个填充。"
  [var-name origin]
  (when (nil? origin)
    (throw (ex-info "phi requires non-nil origin"
                    {:var-name var-name})))
  (->Phi var-name origin []))

(defn phi? [x] (instance? Phi x))
(defn phi-var-name [^Phi p] (:var-name p))
(defn phi-origin [^Phi p] (:origin p))
(defn phi-inputs [^Phi p] (:inputs p))

(defn phi-substs [^Phi phi]
  (let [phi-var-name (phi-var-name phi)]
    (reduce
      (fn [substs {:keys [var-name]}]
        (if-not (= phi-var-name var-name)
          (assoc substs var-name phi-var-name)
          substs))
      {}
      (phi-inputs phi))))

(defn rename-phi-var-name [^Phi p new-var-name]
  (assoc p :var-name new-var-name))

(defn update-phi-input
  "更新或添加 phi 在 predecessor-id 的输入。

   origin   —— 该前驱块传入的值节点（variable ref）
   var-name —— variable 的 name（带 SSA 版本号）

   已有该 predecessor-id 的 input 时更新，否则添加。"
  [^Phi p predecessor-id origin var-name]
  (let [new-input (make-input predecessor-id origin var-name)]
    (update p :inputs
            (fn [inputs]
              (if (some #(= predecessor-id (:predecessor-id %)) inputs)
                (mapv (fn [input]
                        (if (= predecessor-id (:predecessor-id input))
                          new-input
                          input))
                      inputs)
                (conj inputs new-input))))))