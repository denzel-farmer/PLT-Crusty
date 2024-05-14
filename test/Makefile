.PHONY: test

test:
	python3 tests.py

test-checksemant:
	python3 test_files.py ../src/checksemant

.PHONY: clean
clean:
	find . -type f \( -name "*.log" -o -name "*.log.sem" -o -name "*.out" -o -name "*.out.llvm" -o -name "*.out.sem" \) -delete 