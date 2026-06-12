.PHONY: clean compile test jar uberjar repl

VERSION := 0.1.0
JAR_FILE := target/homunculus-$(VERSION).jar

all: test

clean:
	clj -T:build clean

# 编译所有源文件，生成 record 类等
compile:
	clj -T:build compile

# 运行所有测试：先编译，再执行所有测试命名空间
test: compile
	clj -M:test \
	  -e "(require 'clojure.test)" \
	  -e "(require '[top.kzre.homunculus.core.ir1.core-test :as ir1t])" \
	  -e "(require '[top.kzre.homunculus.core.ir2.core-test :as ir2t])" \
	  -e "(require '[top.kzre.homunculus.core.ir2.forms-test :as ir2ft])" \
	  -e "(require '[top.kzre.homunculus.core.types.infer.core-test :as infer])" \
	  -e "(require '[top.kzre.homunculus.core.types.typed.core-test :as typed])" \
	  -e "(require '[top.kzre.homunculus.core.types.typed.extra-test :as extra])" \
	  -e "(require '[top.kzre.homunculus.core.types.typed.let-poly-test :as letpoly])" \
	  -e "(require '[top.kzre.homunculus.core.types.typed.let-poly-comprehensive-test :as letpolycomp])" \
	  -e "(require '[top.kzre.homunculus.core.types.typed.call-unit-test :as callunit])" \
	  -e "(require '[top.kzre.homunculus.core.types.typed.variable-let-unit-test :as varlet])" \
	  -e "(require '[top.kzre.homunculus.core.types.check.core-test :as check])" \
	  -e "(require '[top.kzre.homunculus.core.types.inline-lift.core-test :as inlinelift])" \
	  -e "(require '[top.kzre.homunculus.core.types.integration-all-test :as integration])" \
	  -e "(clojure.test/run-tests 'ir1t 'ir2t 'ir2ft 'infer 'typed 'extra 'letpoly 'letpolycomp 'callunit 'varlet 'check 'inlinelift 'integration)" \
	  -e "(println \"All tests done.\")"

jar: compile
	clj -T:build jar

uberjar: compile
	clj -T:build uberjar

repl:
	clj -M:dev