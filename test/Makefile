.PHONY: test
test: test-automated

test-automated:
	python3 tests.py

test-checksemant:
	python3 samples-manual/old_tester.py ../checksemant.native

.PHONY: clean
clean:
	find . -type f \( -name "*.log" -o -name "*.log.sem" -o -name "*.out" -o -name "*.out.llvm" -o -name "*.out.sem" \) ! -name "*.expected.out" -delete 