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

ast.cmo : \
    ast.cmi
ast.cmx : \
    ast.cmi
ast.cmi :
astprint.cmo : \
    ast.cmi \
    astprint.cmi
astprint.cmx : \
    ast.cmx \
    astprint.cmi
astprint.cmi : \
    ast.cmi
checksemant.cmo : \
    semant.cmo \
    scanner.cmo \
    sastprint.cmi \
    psuedotranslate.cmo \
    linear.cmo \
    crustyparse.cmi \
    astprint.cmi
checksemant.cmx : \
    semant.cmx \
    scanner.cmx \
    sastprint.cmx \
    psuedotranslate.cmx \
    linear.cmx \
    crustyparse.cmx \
    astprint.cmx
crusty.cmo : \
    semant.cmo \
    scanner.cmo \
    sastprint.cmi \
    irgen.cmo \
    crustyparse.cmi \
    astprint.cmi
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
    ast.cmx \
    crustyparse.cmi
crustyparse.cmi : \
    ast.cmi
irgen.cmo : \
    sast.cmi \
    ast.cmi
irgen.cmx : \
    sast.cmx \
    ast.cmx
linear.cmo : \
    sastprint.cmi \
    sast.cmi \
    log.cmi \
    astprint.cmi \
    ast.cmi
linear.cmx : \
    sastprint.cmx \
    sast.cmx \
    log.cmx \
    astprint.cmx \
    ast.cmx
log.cmo : \
    log.cmi
log.cmx : \
    log.cmi
log.cmi :
sast.cmo : \
    ast.cmi \
    sast.cmi
sast.cmx : \
    ast.cmx \
    sast.cmi
sast.cmi : \
    ast.cmi
sastprint.cmo : \
    sast.cmi \
    ast.cmi \
    sastprint.cmi
sastprint.cmx : \
    sast.cmx \
    ast.cmx \
    sastprint.cmi
sastprint.cmi : \
    sast.cmi \
    ast.cmi
scanner.cmo : \
    crustyparse.cmi
scanner.cmx : \
    crustyparse.cmx
scanparse.cmo : \
    scanner.cmo \
    crustyparse.cmi \
    astprint.cmi
scanparse.cmx : \
    scanner.cmx \
    crustyparse.cmx \
    astprint.cmx
semant.cmo : \
    sast.cmi \
    astprint.cmi \
    ast.cmi
semant.cmx : \
    sast.cmx \
    astprint.cmx \
    ast.cmx
semantparse.cmo : \
    scanner.cmo \
    crustyparse.cmi
semantparse.cmx : \
    scanner.cmx \
    crustyparse.cmx
structs.cmo :
structs.cmx :
##############################


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo _build checksemant.native crustyparse.ml crustyparse.mli crustyparse.output scanner.ml scanparse hello.out crustyparse.conflicts checksemant