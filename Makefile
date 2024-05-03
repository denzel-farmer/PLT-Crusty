all: clean test scanparse

.PHONY: test
test: scanparse
	make test -C test

scanparse:
	make scanparse -C src

.PHONY: clean
clean:
	make clean -C src
	make clean -C test
