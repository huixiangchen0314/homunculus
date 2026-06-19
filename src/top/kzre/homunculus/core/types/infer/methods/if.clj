(ns top.kzre.homunculus.core.types.infer.methods.if
  (:require [top.kzre.homunculus.core.types.infer.core :as c]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]
            [top.kzre.homunculus.core.types.protocol :as proto]))

(defn- infer-optional-branch [branch context]
  (if branch
    (c/local-infer branch context)
    [nil nil context]))

(defmethod c/local-infer :if [node context]
  (let [frontend   (:frontend context)
        truly-type (proto/truly-type frontend)
        ;; 1. 推断 test
        [test-ty test-node test-ctx] (c/local-infer (n/if-test node) context)
        ;; 2. 推断 then
        [then-ty then-node then-ctx] (c/local-infer (n/if-then node) test-ctx)
        ;; 3. 推断 else（如果有）
        [else-ty else-node else-ctx] (infer-optional-branch (n/if-else node) then-ctx)
        final-ctx else-ctx]
    ;; test 类型是否满足语言要求（若指定了真值类型）
    (if (or (nil? truly-type)
            (and test-ty (t/type=? test-ty (t/make-tcon truly-type))))
      ;; test 类型通过，检查分支类型一致性
      (if (and then-ty
               (or (not (n/if-else node))
                   (t/type=? then-ty else-ty)))
        (c/success then-ty
                   (-> node
                       (n/if-with-children test-node then-node else-node)
                       (t/set-type! then-ty))
                   final-ctx)
        (c/nothing (n/if-with-children node test-node then-node else-node) final-ctx))
      ;; test 类型不符合要求
      (c/nothing (n/if-with-children node test-node then-node else-node) final-ctx))))