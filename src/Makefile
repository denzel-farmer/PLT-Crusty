all : scanparse helloworld checksemant

##############################
#
# HW 2: Question 1
#
# Compilation: 
# Option 1: Simply type "make" to compile the calculator (recommended, auto-test included)
# Option 2: "ocamlbuild calc.native" will also build the calculator

# For testing, you can run the binary executable and test it with
# standard input via terminal.
# Or use calc.tb (testbench file): you can modify the file directly
# with the exprssion you want to test before make. After compiling
# your executable successfully, the output of test case will be 
# generate automatically in a file named calc.out

checksemant: sastprint.cmo linear.cmo astprint.cmo scanner.cmo crustyparse.cmo semant2.cmo checksemant.cmo
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
sast.cmi : \
    ast.cmi

astprint.cmo : \
    ast.cmi
astprint.cmx : \
    ast.cmi
checksemant.cmo : \
    semant2.cmo \
    sastprint.cmo \
    linear.cmo \
    astprint.cmo
checksemant.cmx : \
    semant2.cmx \
    sastprint.cmx \
    linear.cmx \
    astprint.cmx
crusty.cmo : \
    semant.cmo \
    sast.cmi \
    astprint.cmo
crusty.cmx : \
    semant.cmx \
    sast.cmi \
    astprint.cmx
linear.cmo : \
    sast.cmi \
    ast.cmi
linear.cmx : \
    sast.cmi \
    ast.cmi
sastprint.cmo : \
    sast.cmi \
    astprint.cmo \
    ast.cmi
sastprint.cmx : \
    sast.cmi \
    astprint.cmx \
    ast.cmi
scanparse.cmo : \
    astprint.cmo
scanparse.cmx : \
    astprint.cmx
semant.cmo :
semant.cmx :
semant2.cmo : \
    sast.cmi \
    astprint.cmo \
    ast.cmi
semant2.cmx : \
    sast.cmi \
    astprint.cmx \
    ast.cmi

##############################


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo crustyparse.ml crustyparse.mli crustyparse.output scanner.ml scanparse hello.out crustyparse.conflicts checksemant