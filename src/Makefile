all : checksemant.native checksemant

# TODO remove this makefile entirely, it is obsolete
# only used for ocamllsp which needs intermeidate products


# New target, using ocamlbuild 
checksemant.native: clean
	ocamlbuild checksemant.native

# Old targets, to generate intermediate files for ocamllsp
checksemant.lsp: sastprint.cmo astprint.cmo linear.cmo scanner.cmo crustyparse.cmo semant.cmo checksemant.cmo
	ocamlc -w A -o checksemant $^

scanparse : crustyparse.cmo scanner.cmo astprint.cmo scanparse.cmo
	ocamlc -w A -o scanparse $^

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

scanner.ml : scanner.mll
	ocamllex $^

crustyparse.ml crustyparse.mli : crustyparse.mly ast.cmi crustyparse.cmi
	menhir --infer --explain $<

helloworld: scanparse hello.cp 
	./scanparse < hello.cp
# > hello.out


# Extra dependencies not found by ocamldep
scanner.cmo : crustyparse.cmo

# Dependencies from ocamldep
ast.cmi :
crustyparse.cmi : \
    ast.cmi
linear.cmi : \
    sast.cmi \
    ast.cmi
sast.cmi : \
    ast.cmi
astprint.cmo : \
    ast.cmi \
    astprint.cmi
astprint.cmx : \
    ast.cmi \
    astprint.cmi
checksemant.cmo : \
    semant.cmo \
    scanner.cmo \
    sastprint.cmo \
    linear.cmi \
    crustyparse.cmi \
    astprint.cmo
checksemant.cmx : \
    semant.cmx \
    scanner.cmx \
    sastprint.cmx \
    linear.cmx \
    crustyparse.cmx \
    astprint.cmx
crusty.cmo : \
    semant.cmo \
    scanner.cmo \
    sastprint.cmo \
    irgen.cmo \
    crustyparse.cmi \
    astprint.cmo
crusty.cmx : \
    semant.cmx \
    scanner.cmx \
    sastprint.cmx \
    irgen.cmx \
    crustyparse.cmx \
    astprint.cmx
crustyparse.cmo : \
    ast.cmi \
    crustyparse.cmi
crustyparse.cmx : \
    ast.cmi \
    crustyparse.cmi
irgen.cmo : \
    sast.cmi \
    ast.cmi
irgen.cmx : \
    sast.cmi \
    ast.cmi
irgen2.cmo :
irgen2.cmx :
linear.cmo : \
    sast.cmi \
    ast.cmi \
    linear.cmi
linear.cmx : \
    sast.cmi \
    ast.cmi \
    linear.cmi
sastprint.cmo : \
    sast.cmi \
    astprint.cmo \
    ast.cmi \
    sastprint.cmi
sastprint.cmx : \
    sast.cmi \
    astprint.cmx \
    ast.cmi \
    sastprint.cmi
scanner.cmo : \
    crustyparse.cmi
scanner.cmx : \
    crustyparse.cmx
scanparse.cmo : \
    scanner.cmo \
    crustyparse.cmi \
    astprint.cmo
scanparse.cmx : \
    scanner.cmx \
    crustyparse.cmx \
    astprint.cmx
semant.cmo : \
    sast.cmi \
    astprint.cmo \
    ast.cmi
semant.cmx : \
    sast.cmi \
    astprint.cmx \
    ast.cmi
semantparse.cmo : \
    scanner.cmo \
    crustyparse.cmi
semantparse.cmx : \
    scanner.cmx \
    crustyparse.cmx
##############################


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo _build checksemant.native crustyparse.ml crustyparse.mli crustyparse.output scanner.ml scanparse hello.out crustyparse.conflicts checksemant