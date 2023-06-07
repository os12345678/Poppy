Type Checking TODO:  
- Define a context  
- Define a type-checking function  
- Implement type checking for each expression type  
- Type-check function and class definitions  
- Type-check the entire program  

Implementing type checking typically involves walking your abstract syntax tree (AST) and checking each node against a set of rules. These rules ensure that the types of the expressions are consistent with their usage in the program. For example, you would check that the operands of arithmetic operators are of numeric types, and that function calls match the declared types of the function's parameters and return type.

Here is a rough guide on how to start implementing type checking for your language:

Define a context: You need to define a context or an environment that maps variables to their types. This context is updated when you enter new scopes (e.g., when entering a function body, where the function parameters introduce new variables).

Define your type-checking function: This function should take an expression and a context, and return a type if the expression is well-typed, or an error otherwise. This function will be a big match statement that checks the type of each kind of expression in your language. For example, if you're type-checking a binary operation, you would recursively type-check the operands, make sure they're of a type that's valid for the operator, and then return the resulting type of the operation.

Implement type checking for each expression type: For each kind of expression in your language, define a function that performs type checking for that expression.

Type-check function and class definitions: Similar to expressions, you will also need to type-check function and class definitions. This typically involves checking that the body of the function or method is type-correct when the function parameters or method parameters and fields are added to the context.

Type-check the entire program: Finally, you can type-check the entire program by type-checking the list of expressions in the program type.