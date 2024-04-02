all : test hello.out

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


test : crustyparse.cmo scanner.cmo astprint.cmo test.cmo
	ocamlc -w A -o test $^

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

scanner.ml : scanner.mll
	ocamllex $^

crustyparse.ml crustyparse.mli : crustyparse.mly ast.cmi
	menhir --infer $<

hello.out: test hello.cp 
	./test < hello.cp > hello.out

# Dependencies from ocamldep
test.cmo : scanner.cmo crustyparse.cmi ast.cmi
test.cmx : scanner.cmx crustyparse.cmx ast.cmi

crustyparse.cmo : ast.cmi crustyparse.cmi
crustyparse.cmx : ast.cmi crustyparse.cmi

scanner.cmo : crustyparse.cmi
scanner.cmx : crustyparse.cmx


# ast.cmi ast.cmo : ast.ml
# 	ocamlc -w A -c $<

##############################


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo crustyparse.ml crustyparse.mli crustyparse.output scanner.ml test hello.out