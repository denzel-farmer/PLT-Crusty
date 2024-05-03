all : scanparse helloworld

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


scanparse : crustyparse.cmo scanner.cmo astprint.cmo scanparse.cmo
	ocamlc -w A -o scanparse $^

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

scanner.ml : scanner.mll
	ocamllex $^

crustyparse.ml crustyparse.mli : crustyparse.mly ast.cmi
	menhir --infer --explain $<

helloworld: scanparse hello.cp 
	./scanparse < hello.cp > hello.out

# Dependencies from ocamldep
scanparse.cmo : scanner.cmo crustyparse.cmi ast.cmi
scanparse.cmx : scanner.cmx crustyparse.cmx ast.cmi

crustyparse.cmo : ast.cmi crustyparse.cmi
crustyparse.cmx : ast.cmi crustyparse.cmi

scanner.cmo : crustyparse.cmi
scanner.cmx : crustyparse.cmx


# ast.cmi ast.cmo : ast.ml
# 	ocamlc -w A -c $<

##############################


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo crustyparse.ml crustyparse.mli crustyparse.output scanner.ml scanparse hello.out crustyparse.conflicts