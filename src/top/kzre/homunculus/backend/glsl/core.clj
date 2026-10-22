(ns top.kzre.homunculus.backend.glsl.core
  "大部分优化和pass都在 ShaderAST 完成，到GLSL 后，
  类型直接降级为字符串，不在 GlslAST 做复杂pass"
  (:require
   [top.kzre.homunculus.backend.glsl.emitter :as emitter]
   [top.kzre.homunculus.backend.hlsl.backend :as backend]
   [top.kzre.homunculus.backend.shader.frontend :as frontend]
   [top.kzre.homunculus.compilers.firstorder :as firstorder]
   [top.kzre.homunculus.internal.compile-target :as compile-target]))

(defonce ^:private target*
         (compile-target/make-compile-target
           (frontend/frontend)
           (backend/backend)
           (firstorder/compiler)
           (emitter/emitter)))

(defn target [] target*)