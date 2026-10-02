.PHONY: clean compile test jar uberjar repl

VERSION := 0.1.0
JAR_FILE := target/homunculus-$(VERSION).jar

all: test

clean:
	clj -T:build clean

# test 自动发现 test 目录下所有 *_test.clj 文件
test: jar
	clj -M:test test/run_tests.clj

jar:
	clj -T:build jar

uberjar:
	clj -T:build uberjar

repl:
	clj -M:dev