(ns top.kzre.homunculus.core.ir2.forms.try
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir1.core :as ir1]))

(defmethod ir2/lower-ast :try [ir1-vec env]
  ;; IR1 :try 向量: [node body... catch... finally...]
  (let [body-irs    (take-while #(not= (:kind (first %)) :catch) (rest ir1-vec))
        catch-irs   (filter #(= (:kind (first %)) :catch) (rest ir1-vec))
        finally-irs (drop (+ (count body-irs) (count catch-irs)) (rest ir1-vec))
        body        (mapv #(first (ir2/lower-ast % env)) body-irs)
        catches     (mapv (fn [c]
                            (let [c-node (first c)
                                  class   (:class c-node)
                                  sym     (:sym c-node)
                                  c-body-irs (drop 2 c)] ; 跳过 catch 节点本身和 class、sym
                              (ir2/catch-expr class sym
                                              (mapv #(first (ir2/lower-ast % env)) c-body-irs))))
                          catch-irs)
        finally     (when (seq finally-irs)
                      (mapv #(first (ir2/lower-ast % env)) finally-irs))
        meta        (ir2/ir1-meta ir1-vec)]
    [(ir2/try-expr body catches finally meta)]))