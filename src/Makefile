all : scanner.ml parser.ml parser.mli

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


# calc : parser.cmo scanner.cmo calc.cmo
# 	ocamlc -w A -o calc $^

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : crustyparse.mly
	menhir --infer --ocamlc 'ocamlfind ocamlc' $^

# calc.out : calc calc.tb
# 	./calc < calc.tb > calc.out

# Depedencies from ocamldep
# calc.cmo : scanner.cmo parser.cmi ast.cmi
# calc.cmx : scanner.cmx parser.cmx ast.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx


##############################


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml calc.out calc