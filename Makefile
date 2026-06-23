.PHONY: clean compile test jar uberjar repl

VERSION := 0.1.0
JAR_FILE := target/homunculus-$(VERSION).jar

all: test

clean:
	clj -T:build clean

compile:
	clj -T:build compile-clj

# test 自动发现 test 目录下所有 *_test.clj 文件
test: compile
	clj -M:test test/run_tests.clj

jar: compile
	clj -T:build jar

uberjar: compile
	clj -T:build uberjar

repl:
	clj -M:dev