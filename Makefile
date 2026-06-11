.PHONY: clean compile test jar uberjar repl

VERSION := 0.1.0
JAR_FILE := target/homunculus-$(VERSION).jar

all: test

clean:
	clj -T:build clean

# 编译所有源文件，生成 record 类等
compile:
	clj -T:build compile

# 运行测试：先编译，再执行测试（所有测试命名空间）
test: compile
	clj -M:test -e "(require 'clojure.test)" \
	             -e "(require '[top.kzre.homunculus.backends.hlsl.ir3-hlsl-test :as hlsl])" \
	             -e "(clojure.test/run-tests hlsl)" \
	             -e "(println \"All tests done.\")"

jar: compile
	clj -T:build jar

uberjar: compile
	clj -T:build uberjar

repl:
	clj -M:dev