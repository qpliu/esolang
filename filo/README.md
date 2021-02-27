FILO is a stack-based programming language.  A FILO program consists of an
expression.

Syntax
======
```ebnf
program = expression ;

expression = "@"
           | "0"
           | expression, "*", expression
           | expression, "+", expression
           | expression, "-", expression
           | identifier, "*", identifier, expression, ",", expression, ","
	   | "[", definitions, "]", expression ;

definitions = definition, { definitions } ;

definition = identifier, "=", expression, "." ;
```
Identifiers are consecutive sequences of non-whitespace characters, excluding
`@`, `0`, `*`, `+`, `-`, `=`, `.`, and `,`.  Whitespace separates identifiers
and is otherwise ignored.

`*`, `+`, and `-` are right-associative with equal precedence.

A `.` immediately preceding `]` may be omitted.

A `,` immediately preceding `]`, `.`, or EOF may be omitted.

Comments begin with `==` and extend to the end of the line.

Expressions
===========
Argument
--------
```
@
```
The argument of the function.

Empty
-----
```
0
```
An empty stack.

Push
----
```
x * y
```
A new stack with `x` pushed onto `y`.

Pop
---
```
x - y
```
The `x` with its top element removed unless `x` is empty, then `y`.

Top
---
```
x + y
```
The top element of `x` unless `x` is empty, then `y`.

Apply
-----
```
f * g x, y,
```
A new stack with the result of `f` applied to the top element of `x` pushed
onto the result of `g` applied to `x` with its top element removed unless
`x` is empty, then `y`.

Let
---
```
[f=x.g=y]z
```
Defines `f` and `g` when evaluating `z`.  This is a recursive let,
so the definitions of `f` and `g` apply to `x` and `y`.

Input/Output encoding
=====================
How input and output are encoded are implementation-defined.

One possibility is for the argument and result to be interpreted as stacks
of bits, where the each element of the stack is interpreted as 0 if it is
empty, and interpreted as 1 if it is not empty, and the argument being
constructed from the bits of the input, and the result being interpreted as
the bits of the output.

Examples
========
These examples use the input/output encoding defined by the implementation
below.

cat
---
```
@
```

Hello world
-----------
```
[h=0*0*0*@*0*0*@*0 * @*0*@*0*0*@*@*0 * 0*0*@*@*0*@*@*0 * 0*0*@*@*0*@*@*0
  *@*@*@*@*0*@*@*0 * 0*0*0*0*0*@*0*0 * @*@*@*0*@*@*@*0 * @*@*@*@*0*@*@*0
  *0*@*0*0*@*@*@*0 * 0*0*@*@*0*@*@*0 * 0*0*@*0*0*@*@*0 * @*0*0*0*0*@*0*0
  *0*@*0*@*0*0*0*0] h*h 0*0*0,0,-0
```

Implementation
==============
To build the [interpreter](filo.hs):
```
ghc --make filo
```
To run the hello world example with the interpreter:
```
./filo ./examples/hello.filo
```
