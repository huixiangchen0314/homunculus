(ns top.kzre.homunculus.core.irstmt.pass.liveness
  "基于结构化控制流的活跃变量分析。"
  (:require
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

(declare live-in live-in-stmt live-in-block expr-read-vars)

(def ^:private stmt-kinds
  #{:block :var-decl :assign :while :if :try :catch :throw
    :record :protocol :ns :protocol-impl :function
    :field :param :method :pair})

;; ── 表达式读取的变量 ──
(defn- expr-read-vars
  "表达式 node 直接读取的变量名集合。"
  [node]
  (cond
    (nil? node) #{}
    (= :variable (ast/kind node)) #{(:name node)}
    (= :literal (ast/kind node)) #{}
    (contains? stmt-kinds (ast/kind node)) (live-in node #{})
    :else
    (reduce (fn [s c] (into s (expr-read-vars c)))
            #{}
            (ast/children node))))

;; ── 左值分析 ──
(defn- lval-defined-name [node]
  (when (and node (= :variable (ast/kind node)))
    (:name node)))

(defn- lval-read-vars [node]
  (cond
    (nil? node) #{}
    (= :variable (ast/kind node)) #{}
    :else (expr-read-vars node)))

;; ── 语句 live-in ──
(defn- live-in-var-decl [stmt live-out]
  (let [name (:name stmt)
        init (:val stmt)]
    (-> live-out
        (disj name)
        (into (if init (live-in init #{}) #{})))))

(defn- live-in-assign [stmt live-out]
  (let [var     (:var stmt)
        val     (:val stmt)
        def-nm  (lval-defined-name var)
        lval-rd (lval-read-vars var)
        val-rd  (if val (live-in val #{}) #{})]
    (cond-> live-out
            ;; assign 的 target 是"使用"（引用已声明变量），不从 live-in 移除。
            ;; 若移除，前置 var-decl 会被误判为死代码删除，
            ;; 导致 assign 引用未声明变量。
            :always (into lval-rd)
            :always (into val-rd)
            def-nm  (conj def-nm))))

(defn- live-in-while [stmt live-out]
  (let [test    (:test stmt)
        body    (:body stmt)
        test-rd (expr-read-vars test)]
    (loop [live (into live-out test-rd)
           n    0]
      (when (> n 10000)
        (throw (ex-info "liveness fixpoint did not converge" {:while stmt})))
      ;; 单调递增：live' ⊇ live，保证收敛
      (let [live' (into live (if body (live-in body live) #{}))]
        (if (= live live')
          live'
          (recur live' (inc n)))))))

(defn- live-in-if [stmt live-out]
  (let [test    (:test stmt)
        then    (:then stmt)
        else    (:else stmt)
        test-rd (expr-read-vars test)
        then-in (if then (live-in then live-out) live-out)
        else-in (if else (live-in else live-out) live-out)]
    (into (into then-in else-in) test-rd)))

(defn- live-in-catch [catch-node live-out]
  (let [sym     (:sym catch-node)
        body    (:body catch-node)
        body-in (if body (live-in body live-out) live-out)]
    (if sym (disj body-in sym) body-in)))

(defn- live-in-try [stmt live-out]
  (let [body    (:body stmt)
        catches (:catches stmt)
        finally (:finally stmt)
        fin-in  (if finally (live-in finally live-out) live-out)
        catch-ins
        (loop [cs      (reverse catches)
               next-in fin-in
               acc     []]
          (if-let [c (first cs)]
            (let [c-in (live-in-catch c next-in)]
              (recur (rest cs)
                     (into fin-in c-in)
                     (conj acc c-in)))
            acc))
        catch-1-in (first catch-ins)
        body-out   (if catch-1-in (into fin-in catch-1-in) fin-in)
        body-in    (if body (live-in body body-out) body-out)]
    body-in))

(defn- live-in-throw [stmt _live-out]
  (let [expr (:expr stmt)]
    (if expr (live-in expr #{}) #{})))

(defn- live-in-function [node _live-out]
  (let [params  (set (map :name (:params node)))
        body    (:body node)
        body-in (if body (live-in body #{}) #{})]
    (reduce disj body-in params)))

(defn- live-in-method [node _live-out]
  (let [params  (set (map :name (:params node)))
        body    (:body node)
        body-in (if body (live-in body #{}) #{})]
    (reduce disj body-in params)))

(defn- live-in-protocol-impl [node _live-out]
  (reduce (fn [acc meth]
            (into acc (live-in-method meth #{})))
          #{}
          (:methods node)))

(defn- live-in-record [node _live-out]
  (let [fields (set (map :name (:fields node)))]
    (reduce (fn [acc impl]
              (into acc
                    (reduce (fn [acc2 meth]
                              (into acc2
                                    (reduce disj
                                            (live-in-method meth #{})
                                            fields)))
                            #{}
                            (:methods impl))))
            #{}
            (:protocols node))))

(defn- live-in-protocol [_node _live-out] #{})
(defn- live-in-ns [_node _live-out] #{})

(defn- live-in-pair [node _live-out]
  (into (let [k (:key node)] (if k (expr-read-vars k) #{}))
        (let [v (:val node)] (if v (expr-read-vars v) #{}))))

(defn- live-in-block [node live-out]
  (let [ret   (:ret node)
        stmts (:stmts node)
        live-after-ret (if ret (live-in ret live-out) live-out)]
    (reduce (fn [live stmt] (live-in stmt live))   ; ← 用 live-in
            live-after-ret
            (reverse stmts))))

(defn live-in-stmt
  "语句节点在给定 live-out 下的 live-in。

   ⚠️ 本函数只应由 live-in 调用（live-in 会先做 stmt-kinds 守卫）。
   直接调用时必须保证 node 是 stmt kind，否则走兜底分支。"
  [node live-out]
  (case (ast/kind node)
    :block         (live-in-block node live-out)
    :var-decl      (live-in-var-decl node live-out)
    :assign        (live-in-assign node live-out)
    :while         (live-in-while node live-out)
    :if            (live-in-if node live-out)
    :try           (live-in-try node live-out)
    :catch         (live-in-catch node live-out)
    :throw         (live-in-throw node live-out)
    :function      (live-in-function node live-out)
    :record        (live-in-record node live-out)
    :protocol      (live-in-protocol node live-out)
    :ns            (live-in-ns node live-out)
    :protocol-impl (live-in-protocol-impl node live-out)
    :method        (live-in-method node live-out)
    :field         #{}
    :param         #{}
    :pair          (live-in-pair node live-out)
    ;; 兜底：表达式类节点被误当作语句时，按读取处理
    (into live-out (expr-read-vars node))))

(defn live-in [node live-out]
  (cond
    (nil? node) live-out
    (contains? stmt-kinds (ast/kind node)) (live-in-stmt node live-out)
    :else (into live-out (expr-read-vars node))))

;; ── 可删性 ──
(defn removable?
  "语句在给定 live-out 和 fx? 下是否可删。
   仅判定简单定义语句：var-decl / 简单变量 assign。"
  [stmt live-out fx?]
  (and (not fx?)
       (case (ast/kind stmt)
         :var-decl
         (not (contains? live-out (:name stmt)))
         :assign
         (let [target (:var stmt)]
           (and (= :variable (ast/kind target))
                (not (contains? live-out (:name target)))))
         false)))