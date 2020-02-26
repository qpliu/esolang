Pointless is a point-free variation on [01_](../01_/doc/01_.txt).

A Pointless program is a Pointless expression, which is a function that
takes a list of bits and returns a list of bits.

Syntax
======

Grammar
-------
```ebnf
program = expr, "." ;

expr = compose | let | if | concat | identifier ;

compose = expr, expr ;

let = "LET", binding, { ",", binding }, "IN", expr ;

if = "(", expr, ",", expr, ",", expr, ")" ;

concat = "(", expr, ",", expr, ")" ;

binding = identifier, "=", expr ;
```
Identifiers are consecutive sequences of non-whitespace characters, excluding
"(", ")", ",", "=", and ".".  Also, "LET" and "IN" are reserved and may not
be used as identifiers.  Whitespace separates identifiers and is otherwise
ignored.

Comments begin with "==" and extend to the end of the line.

Expressions
===========

Compose
-------
```
f g
```
This is a function that returns the result of applying f to the result of
appying g to its argument.

Let
---
```
LET f = (_,0 f,1 f) IN f
```
A LET expression binds one or more identifiers, which may be referred to
in the body of the expression or in the bodies of the bindings.  Bindings
in a LET expression may shadow the bindings of any enclosing scope, including
the initial scope.

If
--
```
(f,g,h)
```
If the argument of this function is an empty list, it returns the result
of applying f to an empty list.

If the argument of this function begins with a 0, it returns the result
of applying g to the rest of argument that follows the 0.

If the argument of this function begins with a 1, it returns the result
of applying h to the rest of argument that follows the 1.

Concat
------
```
(f,g)
```
This is a function that returns the concatenation of the result applying f
to its argument and the result of applying g to its argument.

Functions
---------
Functions may be identifiers bound in an enclosing LET expression or a
built-in function bound in the initial scope.  The built-in functions
are as follows.

### _
_ ignores its argument and returns an empty list.

### 0
0 returns its argument with 0 prepended.

### 1
1 returns its argument with 1 prepended.

Examples
========

Hello world
-----------
```
0 0 0 1 0 0 1 0  1 0 1 0 0 1 1 0  0 0 1 1 0 1 1 0  0 0 1 1 0 1 1 0
1 1 1 1 0 1 1 0  0 0 0 0 0 1 0 0  1 1 1 0 1 1 1 0  1 1 1 1 0 1 1 0
0 1 0 0 1 1 1 0  0 0 1 1 0 1 1 0  0 0 1 0 0 1 1 0  1 0 0 0 0 1 0 0
0 1 0 1 0 0 0 0 _.
```

Cat
---
```
LET cat = (_,0 cat,1 cat) IN cat.
```

Implementation
==============
To build the [interpreter](pointless.hs):
```
ghc --make pointless
```
To run the hello world example with the interpreter:
```
./pointless ./examples/hello.pointless
```