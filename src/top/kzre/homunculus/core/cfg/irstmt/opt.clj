(ns top.kzre.homunculus.core.cfg.irstmt.opt
  "CFG 优化总入口——对 IRStmt 的 function/method 做 SSA 全流程。

   ── 作用域 ──

   每个 function/method 独立处理——CFG 不跨函数。
   其他节点（var-decl / ns / record 等）原样返回。

   ── 边界 ──

   bake 当前只支持直线代码和 if 各自 return。while / 汇合 if
   不支持——bake 抛异常时这个 function 返回原 node，保留上游
   assign-propagate + dce 的结果。

   这不是开关——是 pass 的确定性边界。同一输入永远同一输出。
   bake 支持 while / 汇合 if 后，边界自然收窄。"
  (:require
   [top.kzre.homunculus.core.cfg.build :as build]
   [top.kzre.homunculus.core.cfg.irstmt.build :as irstmt-build]
   [top.kzre.homunculus.core.cfg.irstmt.pass.copy-prop :as copy-prop]
   [top.kzre.homunculus.core.cfg.irstmt.varversion :as varversion]
   [top.kzre.homunculus.core.cfg.pass.ssa.construct :as construct]
   [top.kzre.homunculus.core.cfg.pass.ssa.destruct :as destruct]
   [top.kzre.homunculus.core.cfg.pass.ssa.raise-phi :as raise-phi]
   [top.kzre.homunculus.core.irstmt.ast :as ast]))

(defn- optimize-function
  [node adapter]
  (let [body (:body node)]
    (if (or (nil? body) (not= :block (ast/kind body)))
      node
      (let [builder  (irstmt-build/builder)
            cfg      (build/build-cfg body builder)
            ssa      (construct/construct cfg adapter)
            restored (destruct/elim-phi ssa adapter)      ; 方法 2 产生 move
            raised   (raise-phi/raise-phi restored adapter) ; phi-var 上浮 ← 插这里
            propped   (copy-prop/copy-prop raised adapter)   ; 剩余拷贝
            cleaned  (varversion/lower propped)]             ; strip + 自赋值
        (assoc node :body cleaned)))))

(defn optimize
  "对 nodes 里所有 function/method 做 CFG 优化。"
  [nodes adapter]
  (mapv (fn [node]
          (case (ast/kind node)
            (:function :method) (optimize-function node adapter)
            node))
        nodes))