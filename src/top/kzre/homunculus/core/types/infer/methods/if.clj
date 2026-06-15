(ns top.kzre.homunculus.core.types.infer.methods.if
  (:require [top.kzre.homunculus.core.types.infer.core :as c]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defn- infer-optional-branch [branch context]
  (if branch
    (c/local-infer branch context)
    [nil nil]))

(defmethod c/local-infer :if [node context]
  (let [[test-ty test-node] (c/local-infer (n/if-test node) context)
        [then-ty then-node] (c/local-infer (n/if-then node) context)
        [else-ty else-node] (infer-optional-branch (n/if-else node) context)]
    (if (t/bool-type? test-ty)
      ;; 必须 then-ty 有效，且 (没有 else 分支 或 else-ty 有效且类型一致)
      (if (and then-ty
               (or (not (n/if-else node))
                   (t/type=? then-ty else-ty)))
        (c/success then-ty
                   (-> node
                       (n/if-with-children test-node then-node else-node)
                       (t/set-type! then-ty)))
        (c/nothing (n/if-with-children node test-node then-node else-node)))
      (c/nothing (n/if-with-children node test-node then-node else-node)))))