(ns top.kzre.homunculus.backend.shader.ast
  "Shader 语言共用 AST结构，以保证统一编译"
  (:require
   [top.kzre.homunculus.core.ast :refer [defast]]))

(declare ShaderAST)

(defast ShaderAST {})
