(ns top.kzre.homunculus.core.types.fold.core
  "常量折叠与传播的迭代入口。"
  (:require [top.kzre.homunculus.core.types.fold.fold :as fd]
            [top.kzre.homunculus.core.types.fold.propagate :as fp]))

(defn make-context
  [compile-ctx frontend backend folder]
  {:ctx      compile-ctx
   :frontend frontend
   :backend  backend
   :folder   folder})

(defn fold
  "循环执行常量折叠和传播，直到 IR 不再变化。"
  [ir2-roots context]
  (let [folder (:folder context)]
    (loop [roots ir2-roots]
      (let [folded   (fd/fold roots folder context)
            prop-ctx (fp/make-context (:ctx context) (:frontend context) (:backend context))
            ;; ★ 修正：使用 :roots 键获取传播后的根列表
            propagated (:roots (fp/propagate folded prop-ctx))]
        (if (= (hash (vec roots)) (hash (vec propagated)))
          propagated
          (recur propagated))))))