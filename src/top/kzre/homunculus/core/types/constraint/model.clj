(ns top.kzre.homunculus.core.types.constraint.model
  "类型约束的数据模型。")
(defrecord CEqual [tvar type])    ; tvar 应与 type 相等
(defrecord COverload [fn-ty-list arg-tys ret-tvar node]) ; 重载消解
(defrecord CConvert [node src-ty dst-ty cost])           ; 隐式转换
