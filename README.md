# The Crusty Language 

# Summary 
This is our final implementation for Crusty, with both semantic type checking and linear type checking included. 

# Building Frontend
To build the semantic checker (which generates a SAST with linearity checking report), run 'ocamlbuild checksemant.native'. 

# Testing
To generate all programs, simply run ocamlbuild -pkgs llvm src/crusty.native, which will build all relevant modules, including the scanner, parser, semantic type checker, linear type checker, and IR generation. These modules are compatible with Ocaml version 4.14.2, Opam version 2.1.5, and 
LLVM verison 14.0.6. 

The crusty.native accepts input (a program) from stdin via a lexbuf stream, runs the code through all the different stages, and eventually sends the output of the IR generation program (assembly-like instructions) to stdout. We used redirection to test our code more easily, writing an initial program in a .crust file that acted as stdin, and sending stdout to a .out file. With the .out file, running lli .out will essentially run the program as if it were an executable. 

For instance, we ran ./crusty.native < basic.crust > basic.out, and then lli basic.out. When running lli basic.out, all print statements then appear in the terminal as when running a standard executable program. 