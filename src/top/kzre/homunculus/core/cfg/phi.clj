(ns top.kzre.homunculus.core.cfg.phi
  "phi 节点的规范形式——SSA 的通用数据结构。

   phi 不是 IRStmt 节点——它只存在于 SSA-CFG 的 BasicBlock 的
   :phis 字段里。:value 是前端的表达式节点（IRStmt 的 variable /
   literal / call），但 phi 结构本身是通用的——所有前端一致，
   不需要协议。

   ── 结构 ────────────────────────────────────────────

   var    —— phi 定义的变量名（symbol）
   inputs —— [{:value expr :pred block-id} ...]
             按前驱块顺序；:value 初始 nil，rename-variables 阶段
             按前驱块的末尾版本填入。

   ── 为什么用 record 而非 map ────────────────────────

   ▸ 类型可辨识——(phi? x) 判断，不会与普通 map 混淆
   ▸ 字段名拼写错误时——(:varr phi) 返回 nil，但 (assoc phi :varr ...)
     进入 extmap，读 :var 拿不到——record 的字段访问是明确定义的，
     拼错容易发现
   ▸ 打印友好——#Phi{:var x$$1 :inputs [...]} 比裸 map 清晰

   ── 不放在 IType / IRStmt 体系里 ────────────────────

   phi 是 SSA 中间产物——construct 后存在，destruct 后消失。
   它不是 IRStmt 节点，不参与 AST 遍历；也不是 IType，不参与
   类型推导。只是 CFG 层的一个数据结构。")

(defrecord Phi
  [var
   inputs]
  ;; var    —— symbol
  ;; inputs —— [{:value expr|nil :pred block-id} ...]
  )

;; ═══════════════════════════════════════════
;; 构造
;; ═══════════════════════════════════════════

(defn make-phi
  "构造 phi。inputs 的 :value 初始为 nil，rename 阶段填入。

   var   —— 变量名符号
   preds —— 前驱块 id 列表（按 CFG 的 preds 顺序）
   ——"
  [var preds]
  (->Phi var (mapv (fn [pred] {:value nil :pred pred}) preds)))

;; ═══════════════════════════════════════════
;; 访问
;; ═══════════════════════════════════════════

(defn phi-var
  [^Phi phi]
  (:var phi))

(defn phi-inputs
  [^Phi phi]
  (:inputs phi))

(defn phi-preds
  "phi 的前驱块 id 列表。"
  [^Phi phi]
  (mapv :pred (:inputs phi)))

;; ═══════════════════════════════════════════
;; 修改
;; ═══════════════════════════════════════════

(defn rename-phi-var
  "重命名 phi 的 var。返回新 Phi。"
  [^Phi phi new-var]
  (assoc phi :var new-var))

(defn update-phi-input
  "更新 pred 对应的输入 value。返回新 Phi。

   pred 不在 phi 的 inputs 里时抛异常——rename-variables 阶段按
   CFG 前驱填 phi 输入，正常不会不匹配。不匹配说明 CFG 结构在
   phi 构造之后被某个 pass 改了但没有同步 phi——这是 bug，
   早点暴露比静默产生错误结果好。"
  [^Phi phi pred new-value]
  (when-not (some #(= (:pred %) pred) (:inputs phi))
    (throw (ex-info "phi input pred not found"
                    {:phi   phi
                     :pred  pred
                     :preds (phi-preds phi)})))
  (update phi :inputs
          (fn [inputs]
            (mapv (fn [input]
                    (if (= (:pred input) pred)
                      (assoc input :value new-value)
                      input))
                  inputs))))

;; ═══════════════════════════════════════════
;; 谓词
;; ═══════════════════════════════════════════

(defn phi?
  [x]
  (instance? Phi x))