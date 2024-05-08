# Generic Checking Rules 
- Need to declare variables before they are used 

- Check that field types in structures exist? (only really matters if it is another structure)
    - doesn't seem like we do this now, should add check in semant that structures actually exist

# Input to Linear Checking

 - sast representing program, containing
    - Structure definitions containing valid fields (i.e. if they are structures, those structures exist)

    
# Linear Checking Rules
- All values in the program are either linear or nonlinear/unrestricted
    - primitive values are, by default, unrestricted 
    - struct values are, by default, linear

- All variables (and function return types) must be declared with an annotation of either linear or unrestricted
    - primitive variable declarations implicitly include an unrestricted annotation
    - struct variable declarations implicitly include a linear annotation
    - constant variables cannot be linear

- Linear variables must be assigned once in the scope in which they are defined, until they are consumed

- A linear value must always be consumed exactly once in the scope in which it was defined after it is assigned
    - Function calls which return linear values must assigned to linear variables 

- Consuming a linear variable means:
    - Assigning it to another variable (must be linear)
        - If it is a struct, this means using the struct explosion operator
    - Passing it as a function parameter (non-borrowed)
    - Returning it from the current function 

- A linear value cannot be assigned to a nonlinear variable 

- A linear value may only be assigned when in the 'unassigned' or 'used' states

## If, Case, and Loop statements
- linear variables delcared outside case, if, and loop statements must exit all branches of the program in the same state (unassigned, assigned, borrowed?, used)
    - So, variables delcared outside loops must be in the 'assigned but unconsumed' state when they exit (either because they were never consumed, or they were consumed an then assigned)

## Borrowing and Ref Types
- The 'borrow' operator (&) may only be used as an argument at a function call site 

- The result of a borrow operator is a ref type, so they can only be used for function args declared as ref types

- Only function arguments can be given reference types (checked by linearity checker)

- A linear variable can only be borrowed if it is in the 'assigned but not consumed' state 

- Unrestricted references can be accessed any number of times either with the deref (*) operator or the arrow (->) operator 


- A ref type cannot be borrowed (i.e. can't 'double borrow')
- A reference to a linear type cannot be dereferenced (with * operator)
- A reference to a linear structure cannot access linear elements with the -> operator
    - However, nonlinear elements of a linear structure may be accessed with the -> operator 


## Structures and Linearity
- Structure definitions do not contain linearity information--an instance of a structure gains linearity properties when it is declared, and its components gain linearity when they are assigned 

- Linear structures can contain both linear and nonlinear components
    - Can even contain no linear components 
- Nonlinear structures cannot be assigned with linear components 
- Nonlinear components of linear structures can be accessed with the dot operator (".")

