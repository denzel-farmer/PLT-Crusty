all: clean test

.PHONY: test
# Test everything
test: checksemant.native crusty.native
	make test -C test

# Build full compiler
crusty.native:
	ocamlbuild -pkgs llvm src/crusty.native

# Build semantic-only checker
checksemant.native: 
	ocamlbuild src/checksemant.native

.PHONY: clean
clean:
	rm -rf _build checksemant.native crusty.native
	make clean -C test
