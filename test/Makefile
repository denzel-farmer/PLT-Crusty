.PHONY: test
test: test-checksemant test-irgen test-automated

# Most comprehensive linear tests
test-automated:
	python3 tests.py

# Older, manual tests 
test-checksemant:
	python3 samples-manual/old_tester.py ../checksemant.native

# irgen-specific tests
test-irgen:
	./irgen.sh

.PHONY: clean
clean:
	find . -type f \( -name "*.log" -o -name "*.log.sem" -o -name "*.out" -o -name "*.out.llvm" -o -name "*.out.sem" \) ! -name "*.expected.out" -delete 