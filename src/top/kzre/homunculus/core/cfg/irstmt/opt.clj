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
    [top.kzre.homunculus.core.cfg.bake :as bake]
    [top.kzre.homunculus.core.cfg.build :as build]
    [top.kzre.homunculus.core.cfg.irstmt.adapt :as irstmt-adapt]
    [top.kzre.homunculus.core.cfg.irstmt.build :as irstmt-build]
    [top.kzre.homunculus.core.cfg.pass.ssa.core :as ssa-core]
    [top.kzre.homunculus.core.irstmt.ast :as ast])
  (:import (clojure.lang ExceptionInfo)))

(defn- optimize-function
  "单个 function/method 的 CFG 优化。

   body 不是 :block 时直接返回。bake 抛异常时返回原 node——
   这个 function 不做 CFG 优化，保留上游优化结果。"
  [node]
  (let [body (:body node)]
    (if (or (nil? body) (not= :block (ast/kind body)))
      node
      (try
        (let [cfg (build/build-cfg body (irstmt-build/builder))
              opt-cfg  (ssa-core/run cfg (irstmt-adapt/adapter))
              baked    (bake/bake cfg (irstmt-adapt/adapter))]
          (assoc node :body baked))
        (catch ExceptionInfo e
          (println "cfg opt failed: " e)
          ;; bake 不支持的结构（while / 汇合 if）——保留原 node
          node)))))

(defn optimize
  "对 nodes 里所有 function/method 做 CFG 优化。"
  [nodes]
  (mapv (fn [node]
          (case (ast/kind node)
            (:function :method) (optimize-function node)
            node))
        nodes))