(ns top.kzre.homunculus.core.types.infer.methods.if
  (:require [top.kzre.homunculus.core.types.infer.core :as c]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defn- infer-optional-branch [branch context]
  "推断可选的 else 分支，若无则返回 [nil nil context] 以保持上下文传递。"
  (if branch
    (c/local-infer branch context)
    [nil nil context]))

(defmethod c/local-infer :if [node context]
  ;; 1. 推断 test 表达式，获得新上下文
  (let [[test-ty test-node test-ctx] (c/local-infer (n/if-test node) context)
        ;; 2. 推断 then 分支，使用 test 后的上下文
        [then-ty then-node then-ctx] (c/local-infer (n/if-then node) test-ctx)
        ;; 3. 推断 else 分支（如果有），使用 then 后的上下文
        [else-ty else-node else-ctx] (infer-optional-branch (n/if-else node) then-ctx)
        ;; 最终上下文来自 else（若无则来自 then）
        final-ctx else-ctx]
    (if (t/bool-type? test-ty)
      ;; 必须 then-ty 有效，且 (没有 else 分支 或 else-ty 有效且类型一致)
      (if (and then-ty
               (or (not (n/if-else node))
                   (t/type=? then-ty else-ty)))
        ;; 成功：返回 then 类型，更新节点，传递最终上下文
        (c/success then-ty
                   (-> node
                       (n/if-with-children test-node then-node else-node)
                       (t/set-type! then-ty))
                   final-ctx)
        ;; then 类型缺失或类型不一致
        (c/nothing (n/if-with-children node test-node then-node else-node) final-ctx))
      ;; test 类型非 bool
      (c/nothing (n/if-with-children node test-node then-node else-node) final-ctx))))