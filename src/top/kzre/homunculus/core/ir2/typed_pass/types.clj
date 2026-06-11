(ns top.kzre.homunculus.core.ir2.typed-pass.types)

(defrecord TVar [id])
(defrecord TCon [name])
(defrecord TFun [arg ret])

(def ^:dynamic *tv-id (atom 0))
(defn fresh-tvar [] (->TVar (swap! *tv-id inc)))

(defn value->type [val]
  (cond
    (instance? java.lang.Long    val) (->TCon :int64)
    (instance? java.lang.Integer val) (->TCon :int32)
    (instance? java.lang.Double  val) (->TCon :float64)
    (instance? java.lang.Float   val) (->TCon :float32)
    (instance? java.lang.Boolean val) (->TCon :bool)
    (instance? java.lang.String  val) (->TCon :string)
    (keyword? val)                    (->TCon :keyword)
    (nil? val)                        (->TCon :nil)
    :else (throw (ex-info "Unsupported literal" {:val val}))))

(defn meta-type [node]
  (when-let [tag (or (get-in node [:meta :tag])
                     (get-in node [:attrs :tag])
                     (get-in node [:attrs :type]))]
    (cond
      (instance? TCon tag) tag
      (symbol? tag) (->TCon (keyword (name tag)))
      (keyword? tag) (->TCon tag)
      (string? tag) (->TCon (keyword tag))
      :else (throw (ex-info "Invalid type tag" {:tag tag})))))