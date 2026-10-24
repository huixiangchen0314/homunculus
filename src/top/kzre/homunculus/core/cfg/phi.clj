(ns top.kzre.homunculus.core.cfg.phi
  "phi 节点的规范形式——SSA 的通用数据结构。

   ── 结构 ────────────────────────────────────────────

   var    —— phi 定义的变量
   inputs —— [{:value expr :pred block-id} ...]
             按前驱块顺序；:value 初始 nil，rename-variables 阶段
             按前驱块的末尾版本填入。
")

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