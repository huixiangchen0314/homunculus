.PHONY: clean uber run

all: clean compile-java compile-clj jar install uber

clean:
	clj -T:build clean

compile-clj:
	clj -T:build compile-clj

compile-java:
	clj -T:build compile-java

jar:
	clj -T:build jar

install:
	clj -T:build install
uber:
	clj -T:build uber


run:
	java -jar target/*.jar