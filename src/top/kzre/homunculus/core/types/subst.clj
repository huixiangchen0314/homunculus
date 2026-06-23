(ns top.kzre.homunculus.core.types.subst
  "替换相关工具函数"
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.protocol :as p]))


(defn inline-call
  [call-node lambda-node _config]
  (let [params (:params lambda-node)
        args (:args call-node)
        body (:body lambda-node)
        subst (zipmap (map :name params) args)
        replace-var (fn replace-var [node]
                      (if (and (satisfies? ir2p/INode node)
                               (= (ir2p/kind node) :variable)
                               (contains? subst (:name node)))
                        (subst (:name node))
                        (case (ir2p/kind node)
                          :call
                          (m/->CallNode (replace-var (:fn node))
                                        (mapv replace-var (:args node))
                                        (:attrs node) (:meta node) (:parent node))
                          :let
                          (m/->LetNode (mapv (fn [[v e]] [(replace-var v) (replace-var e)])
                                             (:bindings node))
                                       (replace-var (:body node))
                                       (:attrs node) (:meta node) (:parent node))
                          :block
                          (m/->BlockNode (mapv replace-var (:exprs node))
                                         (:attrs node) (:meta node) (:parent node))
                          :if
                          (m/->IfNode (replace-var (:test node))
                                      (replace-var (:then node))
                                      (when (:else node) (replace-var (:else node)))
                                      (:attrs node) (:meta node) (:parent node))
                          :loop
                          (m/->LoopNode (mapv (fn [[v e]] [(replace-var v) (replace-var e)])
                                              (:bindings node))
                                        (replace-var (:body node))
                                        (:attrs node) (:meta node) (:parent node))
                          :while
                          (m/->WhileNode (replace-var (:test node))
                                         (replace-var (:body node))
                                         (:attrs node) (:meta node) (:parent node))
                          :try
                          (m/->TryNode (mapv replace-var (:body node))
                                       (mapv replace-var (:catches node))
                                       (when (:finally node) (mapv replace-var (:finally node)))
                                       (:attrs node) (:meta node) (:parent node))
                          :catch
                          (m/->CatchNode (:class node) (:sym node)
                                         (mapv replace-var (:body node))
                                         (:attrs node) (:meta node) (:parent node))
                          :throw
                          (m/->ThrowNode (replace-var (:expr node))
                                         (:attrs node) (:meta node) (:parent node))
                          :assign
                          (m/->AssignNode (replace-var (:var node))
                                          (replace-var (:val node))
                                          (:attrs node) (:meta node) (:parent node))
                          :vector
                          (m/->VectorNode (mapv replace-var (:items node))
                                          (:attrs node) (:meta node) (:parent node))
                          :map
                          (m/->MapNode (mapv replace-var (:kvs node))
                                       (:attrs node) (:meta node) (:parent node))
                          ;; 对于 :literal :variable 及其他未知节点，原样返回
                          node)))]
    (replace-var body)))





;(defn lift-lambda
;  "对 lambda 节点执行闭包提升（Lambda Lifting），将其自由变量转化为显式参数，
; 并创建一个顶层 define 定义，同时返回代表该定义名称的变量引用节点。
;
; 参数：
;   lambda-node — 待提升的 lambda 节点，需包含 :params（形参列表）和 :body（函数体）。
;   free-vars   — 一个自由变量名的集合（如 #{'x 'y}），表示在 lambda 体内引用但未绑定的变量。
;   config      — 编译器配置，用于生成提升后的函数名称（通过 p/lift-name-gen）。
;
; 返回值：
;   一个 map，包含两个键：
;     :define — 新创建的 define 节点，将提升后的 lambda 绑定到一个全局名称。
;     :ref    — 对提升后的函数名的变量引用节点（:variable 节点），可用于替换原 lambda 出现的位置。
;
; 具体步骤：
;   1. 通过 p/lift-name-gen 生成一个新的函数名（如 'closure_xxx）。
;   2. 若 free-vars 非空，对 lambda 进行改造：
;      - 为每个自由变量生成一个 :variable 节点作为额外形参，追加到原 :params 之后。
;      - 构造一个 let 绑定：将每个自由变量名绑定到同名的 :variable 引用（实际上相当于
;        让函数体内部通过 let 引入这些变量，其值由外部传入的同名参数提供）。
;        例如，自由变量为 x、y，则生成 (let [x x, y y] original-body)。
;      - 将原 :body 替换为该 let 节点，并将新的形参列表写入 lambda。
;      - 若 free-vars 为空，则 lambda 保持不变。
;   3. 用提升后的 lambda 创建一个 :define 节点，定义为 lifted-name。
;   4. 创建一个指向 lifted-name 的 :variable 引用节点。
;   5. 返回 {:define define-node, :ref ref-node}。
;
; 设计意图：
;   - 消除闭包中的自由变量，使 lambda 变为纯顶层函数（所有依赖均由参数传入）。
;   - 调用处可以通过额外传入自由变量的值来调用提升后的函数，从而替代原 lambda。
;   - let 引入同名变量的技巧，使得函数体内对自由变量的引用无需修改，只需外部传入同名参数即可。
;
; 注意事项：
;   - 假设 free-vars 中的变量名在调用环境中确实存在同名绑定（由调用方保证）。
;   - 生成的 let 节点可能会引入额外的嵌套层级，但对语义无影响。
;   - 函数名称的生成依赖于 config 中的命名策略，应确保不与其他定义冲突。
;   - 返回的 ref 节点使用 name 作为变量名，可能是一个符号（需与 define 的 :name 一致）。"
;  [lambda-node free-vars  config]
;  (println "lift-lambda: free-vars =" free-vars)
;  (let [lifted-name (p/lift-name-gen config lambda-node)
;        new-lambda (if (seq free-vars)
;                     (let [extra-params (mapv #(m/->VariableNode % nil nil nil) free-vars)
;                           let-bindings (vec (for [fv free-vars]
;                                               [(m/->VariableNode fv nil nil  nil)
;                                                (m/->VariableNode fv nil nil  nil)]))
;                           new-body (m/->LetNode let-bindings (:body lambda-node) nil nil  nil)]
;                       (assoc lambda-node
;                         :params (into (:params lambda-node) extra-params)
;                         :body new-body))
;                     lambda-node)
;        define-node (m/->DefineNode lifted-name new-lambda nil nil nil  nil)
;        ref-node (m/->VariableNode (name lifted-name) nil nil nil)]
;    {:define define-node :ref ref-node}))



(defmulti replace-in-expr
          "在表达式中替换变量引用。"
          (fn [node var-name replacement] (ir2p/kind node)))

(defmethod replace-in-expr :variable [node var-name replacement]
  (if (= (:name node) var-name) replacement node))

(defmethod replace-in-expr :call [node var-name replacement]
  (m/->CallNode (replace-in-expr (:fn node) var-name replacement)
                (mapv #(replace-in-expr % var-name replacement) (:args node))
                (:attrs node) (:meta node) (:parent node)))


(defmethod replace-in-expr :let [node var-name replacement]
  (m/->LetNode (mapv (fn [[v e]] [(replace-in-expr v var-name replacement)
                                  (replace-in-expr e var-name replacement)])
                     (:bindings node))
               (replace-in-expr (:body node) var-name replacement)
               (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :block [node var-name replacement]
  (m/->BlockNode (mapv #(replace-in-expr % var-name replacement) (:exprs node))
                 (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :if [node var-name replacement]
  (m/->IfNode (replace-in-expr (:test node) var-name replacement)
              (replace-in-expr (:then node) var-name replacement)
              (when (:else node) (replace-in-expr (:else node) var-name replacement))
              (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :loop [node var-name replacement]
  (m/->LoopNode (mapv (fn [[v e]] [(replace-in-expr v var-name replacement) (replace-in-expr e var-name replacement)])
                      (:bindings node))
                (replace-in-expr (:body node) var-name replacement)
                (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :while [node var-name replacement]
  (m/->WhileNode (replace-in-expr (:test node) var-name replacement)
                 (replace-in-expr (:body node) var-name replacement)
                 (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :assign [node var-name replacement]
  (m/->AssignNode (replace-in-expr (:var node) var-name replacement)
                  (replace-in-expr (:val node) var-name replacement)
                  (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :try [node var-name replacement]
  (m/->TryNode (mapv #(replace-in-expr % var-name replacement) (:body node))
               (mapv #(when (satisfies? ir2p/INode %) (replace-in-expr % var-name replacement))
                     (:catches node))
               (when (:finally node)
                 (mapv #(replace-in-expr % var-name replacement) (:finally node)))
               (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :catch [node var-name replacement]
  ;; 不处理 class 和 sym，仅递归 body
  (m/->CatchNode (:class node) (:sym node)
                 (mapv #(replace-in-expr % var-name replacement) (:body node))
                 (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :throw [node var-name replacement]
  (m/->ThrowNode (replace-in-expr (:expr node) var-name replacement)
                 (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :vector [node var-name replacement]
  (m/->VectorNode (mapv #(replace-in-expr % var-name replacement) (:items node))
                  (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :map [node var-name replacement]
  (m/->MapNode (mapv #(replace-in-expr % var-name replacement) (:kvs node))
               (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :lambda [node var-name replacement]
  ;; 注意：lambda 内部可能引用 var-name，但因为有新绑定，简单替换不安全，
  ;; 但调用者通常已做 alpha-rename，这里仅递归
  (m/->LambdaNode (mapv #(replace-in-expr % var-name replacement) (:params node))
                  (replace-in-expr (:body node) var-name replacement)
                  (:captures node) (:fn-name node)
                  (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :define [node var-name replacement]
  (m/->DefineNode (:name node)
                  (replace-in-expr (:val node) var-name replacement)
                  (:doc node) (:attrs node) (:meta node) (:parent node)))

(defmethod replace-in-expr :literal [node var-name replacement] node)

(defmethod replace-in-expr :default [node var-name replacement]
  (throw (ex-info (str "Unknown node in inline-expr: " (ir2p/kind node)) {:node node})))

;; 对外接口
(defn inline-expr [node var-name replacement]
  (if (satisfies? ir2p/INode node)
    (replace-in-expr node var-name replacement)
    node))