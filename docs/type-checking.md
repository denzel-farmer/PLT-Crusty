
# Linear Checker Implementation
- Take as input semantic abstract syntax tree, with root program p
- Processes structure definitions to produce structure_info map
    - For each structure, check that no unrestricted struct contains linear var
    - Add linear structures to map from name -> definition
- Process function prototypes to produce function_info map 
    - For each function with linear return or linear args, add a map from name -> ret type, arg types
- Process functions 
    - construct initial linear map from function arguments 
            - Add each linear argument to linear map as state Assigned
            - Add each ref argument to linear map as state Ref
    - Process block (locals, statements)
        - add local declarations
            - Add each linear variable to linear map as state Unassigned
        - process statements (Block, Expr, If, While):
            - For each statement:
                - If Block, recursively process inner block (this enters a new scope)
                - If If
                    - check conditional expression 
                    - produce 'true map' by recursively processing the first statement 
                    - produce 'false map' by recursively processing the second statement
                    - compare true/false maps to ensure all elements in matching states
                - If While
                    - check conditional expression, store result as 'initial map'
                    - produce 'end map' by recursively processing body statement 
                    - compare initial/end maps to ensure all elements in matching states
                - If Expr (Identifier, Literal, Operation, Assignment, Call)
                    - If literal, do nothing
                    - If identifier, try Assigned -> Used  
                    - If operation
                        - If unary operation, check operand expression
                        - If binary operation, check first expr then second expr
                        - If access operation (EXPR.STRING or EXPR->STRING)
                            - If dot 
                                - Get struct name from type of EXPR (if not struct type, raise error)
                                - Check field STRING in struct is unrestricted (if field not found, raise error)
                            - If arrow
                                - Get struct name from type within Ref (if not ref struct type, raise error)
                                - Check field STRING in struct is unrestricted
                        - If Deref
                        - If Borrow
                        - If index
                    - If assignment
                    - If call 

        - remove local declarations, checking that each is in a valid state (Used or Ref, I think)



## Linear Map
- State: Unassigned, Assigned, Borrowed, Used, Ref


- verifies that the sast doesn't break type rules
    - 

    - Every time checker leaves scope, remove variables declared in that scope
    from linear map and check if any removed are not consumed 

## Linear Type System
- All types include a 'Linear' or 'Unrestricted' qualifier 

### Linear Primitive Types 
- When declared, variables with primitive types always include one of these
qualifiers
- Primitive declarations without an 'Unrestricted' or 'Linear' keyword are 
assumed to be unrestricted
    - Because linearity doesn't matter for local primitives (they are managed by
    the stack)

### Linear Structure Types 
- Struct types must include this qualifier in the struct definition itself,
    not when specific instances are declared 
    - So, cannot declare Linear or Unrestricted versions of the same struct
- Struct definitions without an 'Unrestricted' or 'Linear' keyword are
assumed to be Linear
- Linear struct definitions may include both linear and unrestricted components 
    - Unrestricted components of linear structures can be accessed and modified
    using the 'dot' operator ('.') so long as the structure is unconsumed
    - Unrestricted components of references to linear structures can be accessed
    (but not modified) using the 'arrow' operator ('->')

### References
- At a function call site, any variable (linear or unrestricted) may be borrowed
with the '&' operator, and a read-only reference to that variable will be passed
as an argument 
#### Borrowing a variable (at a call site)
- The borrow operator ('&') outputs a value with the same type as the input
variable, but with the "Ref" type qualifier 
- The borrow operator can only be applied to variables, not values (i.e. &(x+1)
is not allowed)
- The borrow operator can only be used at a call site, and only if the value it
outputs is passed directly as an argument to the called function (i.e. foo(1+&x)
is not allowed)
    - The type of the argument must also be a "Ref" type (checked by the regular
    type system)
- Both linear and unrestricted variables can be safely borrowed multiple times,
because borrowing a linear variable does not constitute consuming it
- Cannot borrow any variable before it is declared
- Cannot borrow linear variables after they are consumed
#### Using a Reference (in a function)
- the "Ref" type qualifier may only be specified in the arguments of a function
- References to variables are read-only, and cannot be reassigned 
    - References can't themselves be linear (i.e. "Linear Ref int x_ref" is not allowed) 
- References to unrestricted variables can be accessed/assigned to non-reference
variables using the 'dereference' operator ("*").
    - kinda useless but still supported
    - Can't dereference linear references 
- Components of references to unrestricted structures and unrestricted components
of linear structures can be accessed using the 'arrow' operator ("->").
- References to linear variables cannot be consumed
    - So, the main use case for references is allowing use of unrestricted components 
    of linear structures without consuming that structure. For example, a linear
    File structure might declare a function 'String readstring(Ref struct File f_ref)' 
    to read a string from the file, without consuming the file.
    
- References to linear primitives are allowed, but are mostly useless

# Linear Type Rules 
- Any value with a linear type must be consumed exactly once in its scope
- An expression producing a linear value can't be discarded without assignment

- Borrow operation rules 
    - must be at a call site
    - must be a variable not an expression (might be checked by semant)
- Rules about references 
- Assignment 
    - struct assignment








































# Generic Checking Rules 
- Need to declare variables before they are used 

- Check that field types in structures exist? (only really matters if it is another structure)
    - doesn't seem like we do this now, should add check in semant that structures actually exist

- Make sure that references are read-only (i.e. can't be on the LHS of an assignment)

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

