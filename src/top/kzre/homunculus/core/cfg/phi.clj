(ns top.kzre.homunculus.core.cfg.phi
  "phi 节点——SSA 的通用数据结构。

   {:var sym :template stmt-or-var :inputs [{:value expr :pred id}]}

   :template —— 定义该 phi var 的原始 stmt 或 variable 节点，
                用于 SSA 重命名和 phi 消除时构造新节点（带 attrs）。
                由 collect-defs 收集，phi 构造时传入。
                约定非 nil——nil 在 make-var-ref/make-move 处抛异常。")

(defrecord Phi [var template inputs])

(defn make-phi
  [var template preds]
  (when (nil? template)
    (throw (ex-info "phi requires non-nil template"
                    {:var var})))
  (->Phi var template
         (mapv (fn [pred] {:value nil :pred pred}) preds)))

(defn phi? [x] (instance? Phi x))
(defn phi-var [^Phi p] (:var p))
(defn phi-template [^Phi p] (:template p))
(defn phi-inputs [^Phi p] (:inputs p))
(defn phi-preds [^Phi p] (mapv :pred (:inputs p)))

(defn rename-phi-var [^Phi p new-var]
  (assoc p :var new-var))

(defn update-phi-input [^Phi p pred new-value]
  (when-not (some #(= (:pred %) pred) (:inputs p))
    (throw (ex-info "phi input pred not found"
                    {:phi p :pred pred :preds (phi-preds p)})))
  (update p :inputs
          (fn [inputs]
            (mapv (fn [input]
                    (if (= (:pred input) pred)
                      (assoc input :value new-value)
                      input))
                  inputs))))