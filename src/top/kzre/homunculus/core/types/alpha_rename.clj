(ns top.kzre.homunculus.core.types.alpha-rename
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.utils :as u]))

(defn- rename-node
  [node rename-table]
  (if (satisfies? ir2p/INode node)
    (case (ir2p/kind node)
      :lambda
      (let [old-params (:params node)
            new-params (mapv (fn [p]
                               (let [old-name (:name p)
                                     new-name (u/fresh-name old-name)]
                                 (swap! rename-table assoc old-name new-name)
                                 (assoc p :name new-name)))
                             old-params)
            new-body (rename-node (:body node) rename-table)]
        (m/->LambdaNode new-params new-body (:captures node) (:fn-name node)
                        (:attrs node) (:meta node) (:parent node)))

      :let
      (let [old-bindings (:bindings node)
            new-bindings (mapv (fn [[var val]]
                                 (let [old-name (:name var)
                                       new-name (u/fresh-name old-name)]
                                   (swap! rename-table assoc old-name new-name)
                                   [(assoc var :name new-name)
                                    (rename-node val rename-table)]))
                               old-bindings)
            new-body (rename-node (:body node) rename-table)]
        (m/->LetNode new-bindings new-body (:attrs node) (:meta node) (:parent node)))

      :loop
      (let [old-bindings (:bindings node)
            new-bindings (mapv (fn [[var val]]
                                 (let [old-name (:name var)
                                       new-name (u/fresh-name old-name)]
                                   (swap! rename-table assoc old-name new-name)
                                   [(assoc var :name new-name)
                                    (rename-node val rename-table)]))
                               old-bindings)
            new-body (rename-node (:body node) rename-table)]
        (m/->LoopNode new-bindings new-body (:attrs node) (:meta node) (:parent node)))

      :catch
      (let [old-sym (:sym node)
            old-name (:name old-sym)
            new-name (u/fresh-name old-name)]
        (swap! rename-table assoc old-name new-name)
        (m/->CatchNode (:class node) (assoc old-sym :name new-name)
                       (mapv #(rename-node % rename-table) (:body node))
                       (:attrs node) (:meta node) (:parent node)))

      :variable
      (if-let [new-name (get @rename-table (:name node))]
        (assoc node :name new-name)
        node)

      :call
      (m/->CallNode (rename-node (:fn node) rename-table)
                    (mapv #(rename-node % rename-table) (:args node))
                    (:attrs node) (:meta node) (:parent node))

      :if
      (m/->IfNode (rename-node (:test node) rename-table)
                  (rename-node (:then node) rename-table)
                  (when (:else node) (rename-node (:else node) rename-table))
                  (:attrs node) (:meta node) (:parent node))

      :block
      (m/->BlockNode (mapv #(rename-node % rename-table) (:exprs node))
                     (:attrs node) (:meta node) (:parent node))

      :assign
      (m/->AssignNode (rename-node (:var node) rename-table)
                      (rename-node (:val node) rename-table)
                      (:attrs node) (:meta node) (:parent node))

      :recur
      (m/->RecurNode (mapv #(rename-node % rename-table) (:args node))
                     (:attrs node) (:meta node) (:parent node))

      :try
      (m/->TryNode (mapv #(rename-node % rename-table) (:body node))
                   (mapv #(rename-node % rename-table) (:catches node))
                   (when (:finally node) (mapv #(rename-node % rename-table) (:finally node)))
                   (:attrs node) (:meta node) (:parent node))

      :throw
      (m/->ThrowNode (rename-node (:expr node) rename-table)
                     (:attrs node) (:meta node) (:parent node))

      :while
      (m/->WhileNode (rename-node (:test node) rename-table)
                     (rename-node (:body node) rename-table)
                     (:attrs node) (:meta node) (:parent node))

      :vector
      (m/->VectorNode (mapv #(rename-node % rename-table) (:items node))
                      (:attrs node) (:meta node) (:parent node))

      :map
      (m/->MapNode (mapv #(rename-node % rename-table) (:kvs node))
                   (:attrs node) (:meta node) (:parent node))

      :define
      (m/->DefineNode (:name node) (rename-node (:val node) rename-table)
                      (:doc node) (:attrs node) (:meta node) (:parent node))

      ;; 叶子
      (:literal :symbol)
      node

      ;; 未知抛出
      (throw (ex-info (str "Unknown node kind in alpha-rename: " (ir2p/kind node)) {:node node})))
    node))

(defn rename
  ([node] (rename-node node (atom {})))
  ([node rename-table] (rename-node node rename-table)))