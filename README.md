# The Crusty Language 

# Summary
We have implemented the basic structure, syntax, and semantics for our Crusty
language. Our scanner and parser works for the general components of the language,
but additional work is needed as outlined in the TODOs below.

Our focus moving forward will be to complete the implementation of our language and then to
transition to working on the linear type system checking aspect of it.

# Testing
- `test/testcases.sh`: runs a few basic test cases.
- `src/Makefile`: on complilation, reads the sample program in `hello.cp` as input, scans, parses, and
then pretty prints to `hello.out`.

Additional tests will be added to the `test` directory before final submission.

# TODOs
## General
- Arrays
- Strings
- **Linear type checking**

## scanner.ml
- Special characters
- Escape sequences
- Bitwise operators

## ast.mli
- VOID function return type

## crustyparse.mly
- COLON
- Prefix and postfix increment and decrement operators
- unary plus and minus
- type casting
- more complex assignment operators
- comma operator
- arrays
- allow declaring structs in function body
- allow mixed declaration and assignment
- add returning void
- differentiate struct and arraylit
- explode/consume syntax

## astprint.ml
- differentiate pre/post
