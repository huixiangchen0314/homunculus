.PHONY: clean test jar uberjar repl

VERSION := 0.1.0
JAR_FILE := target/homunculus-$(VERSION).jar

all: test

clean:
	clj -T:build clean

# 运行测试：先确保源命名空间被加载（defrecord 类存在），再执行测试
test:
	clj -T:build compile-hlsl
	clj -M:test -e "(require 'top.kzre.homunculus.backends.hlsl.ir3-hlsl)" \
    	             -e "(require 'top.kzre.homunculus.backends.hlsl.ir3-hlsl-test)" \
    	             -e "(clojure.test/run-tests 'top.kzre.homunculus.backends.hlsl.ir3-hlsl-test)"

jar:
	clj -T:build jar

uberjar:
	clj -T:build uberjar

repl:
	clj -M:dev