(ns top.kzre.homunculus.core.types.constraint.protocol)

;; TODO 携带更多编译时信息
(defprotocol IConstraint
  (solve-constraint [this subst-map env] "求解约束，返回新的替换map")
  (substitute-constraint [this subst-map ] "用替换map对该约束进行替换，返回新约束")
  (solved? [this] "判断该约束在当前替换下是否已解决"))
