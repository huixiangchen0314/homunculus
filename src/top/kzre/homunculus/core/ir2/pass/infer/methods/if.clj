(ns top.kzre.homunculus.core.ir2.pass.infer.methods.if
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as c]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defn- infer-optional-branch
  [branch env]
  (if branch
    (c/infer-node* branch env)
    [nil nil env]))

(defmethod c/infer-node* :if [node env]
  (let [required-type (p/truthy-type env)
        [test-type test-node test-env] (c/infer-node* (n/if-test node) env)
        [then-type then-node then-env] (c/infer-node* (n/if-then node) test-env)
        [else-type else-node else-env] (infer-optional-branch (n/if-else node) then-env)]
    (if (or (nil? required-type)
            (and test-type (t/type=? test-type (t/make-tcon required-type))))
      (if (and then-type
               (or (not (n/if-else node))
                   (t/type=? then-type else-type)))
        (c/success then-type
                   (-> node
                       (n/if-with-children test-node then-node else-node)
                       (t/set-type! then-type))
                   else-env)
        (c/nothing (n/if-with-children node test-node then-node else-node) else-env))
      (c/nothing (n/if-with-children node test-node then-node else-node) else-env))))