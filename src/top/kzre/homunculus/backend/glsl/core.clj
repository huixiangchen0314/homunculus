(ns top.kzre.homunculus.backend.glsl.core
  "大部分优化和pass都在 ShaderAST 完成，到GLSL 后，
  类型直接降级为字符串，不在 GlslAST 做复杂pass"
  (:require
    [top.kzre.homunculus.backend.hlsl.frontend :as frontend]
   [top.kzre.homunculus.backend.hlsl.backend :as backend]
   [top.kzre.homunculus.internal.model :as model]
   [top.kzre.homunculus.compilers.firstorder :as firstorder]
   [top.kzre.homunculus.backend.glsl.emitter :as emitter]
  ))

(defonce ^:private target*
         (model/make-compile-target
           (frontend/frontend)
           (backend/backend)
           (firstorder/compiler)
           (emitter/emitter)))

(defn target [] target*)