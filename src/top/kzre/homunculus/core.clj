(ns top.kzre.homunculus.core)

(defmacro ns [name & references]
  (let [docstring  (when (string? (first references)) (first references))
        references (if docstring (next references) references)
        attr-map   (when (map? (first references)) (first references))
        references (if attr-map (next references) references)]
    `(ns* '~name ~docstring ~attr-map ~(vec references))))