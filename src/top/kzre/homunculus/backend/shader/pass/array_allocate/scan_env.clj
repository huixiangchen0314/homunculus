(ns top.kzre.homunculus.backend.shader.pass.array-allocate.scan-env
  "扫描阶段的环境：累加数组声明与访问点。

   扫描是纯收集过程，不产生新节点。
   环境承载收集结果，随遍历单向累积。

   程序点用前序遍历序号（id）标识，由 IEnv 的 step 方法统一分配。
   下游按 id 大小比较先后顺序。"
  (:require
    [top.kzre.homunculus.backend.shader.ast :as ast]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))


;; ── 表条目 ──

(defrecord ArrayEntry
  [^long id      ; int      声明点程序编号
   name          ; symbol   变量名
   type          ; IR type  数组类型
   ^long size    ; int      槽位数
   elem-type])   ; IR type  元素类型

(defn make-array-entry [id node]
  (let [t (:type node)]
    (->ArrayEntry id
                  (:name node)
                  t
                  (ty/value-val (ty/vec-size t))
                  (ty/vec-element-type t))))

(defrecord AccessEntry
  [^long id          ; int   访问点程序编号
   ^boolean dynamic?])  ; bool 下标是否为动态

(defn make-access-entry [id node]
  (->AccessEntry id
                 (not= :literal (ast/kind (:index node)))))


;; ── 环境 ──

(defprotocol IEnv
  (step [_]                     "步进一个节点，返回 [新env id]")
  (step-array-decl [_ node]     "步进并记录一个数组声明，返回 [新env id]")
  (step-array-access [_ var-name node] "步进并记录一个数组访问，返回 [新env id]")
  (decls [_])
  (accesses [_]))


(defrecord Env [decls accesses counter]
  IEnv
  (step [_]
    (let [id (inc counter)]
      [(->Env decls accesses id) id]))

  (step-array-decl [this node]
    (let [[env' id] (step this)]
      [(update env' :decls conj (make-array-entry id node))
       id]))

  (step-array-access [this var-name node]
    (let [[env' id] (step this)]
      [(update-in env' [:accesses var-name]
                  (fnil conj [])
                  (make-access-entry id node))
       id]))

  (decls [_] decls)
  (accesses [_] accesses))


(defn make-env []
  (->Env [] {} 0))