# Generic Checking Rules 
- Need to declare variables before they are used 

# Linear Checking Rules
- All values in the program are either linear or nonlinear/unrestricted
    - primitive values are, by default, unrestricted 
    - struct values are, by default, linear

- All variables (and function return types) must be declared with an annotation of either linear or unrestricted
    - primitive variable declarations implicitly include an unrestricted annotation
    - struct variable declarations implicitly include a linear annotation
    - constant variables cannot be linear

- Linear variables must be assigned once in the scope in which they are defined 

- A linear value must always be consumed exactly once in the scope in which it was defined
    - Function calls which return linear values must assigned to linear variables 

- Consuming a linear variable means:
    - Assigning it to another variable (must be linear)
        - If it is a struct, this means using the struct explosion operator
    - Passing it as a function parameter (non-borrowed)

- A linear value cannot be assigned to a nonlinear variable 

## If and Case statements
- linear variables delcared outside case or if statements must be consumed the same 
number of times (either 0 or once) in all branches

## Loops 
- linear variables must enter and exit a loop with the same ??

## Borrowing and Ref Types
- The 'borrow' operator (&) may only be used as an argument at a function call site 

- The result of a borrow operator is a ref type, so they can only be used for function args declared as ref types

- Only function arguments can be given reference types

- Unrestricted references can be accessed any number of times either with the deref (*) operator 
or the arrow (->) operator 


- A ref type cannot be borrowed (i.e. can't 'double borrow')
- A reference to a linear type cannot be dereferenced (with * operator)
- A reference to a linear structure cannot access elements with the -> operator
## Structures and Linearity
- Linear structures can contain both linear and nonlinear components
    - Can even contain no linear components 
- Nonlinear structures cannot be assigned with linear components 
- Linear structures cannot  

