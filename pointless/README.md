Pointless is a point-free variation on [01_](../01_/doc/01_.txt).

A Pointless program is a Pointless expression, which is a function that
takes a list of bits and returns a list of bits.

Syntax
======
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
`(`, `)`, `,`, `=`, and `.`.  Also, `LET` and `IN` are reserved and may not
be used as identifiers.  Whitespace separates identifiers and is otherwise
ignored.

Comments begin with `==` and extend to the end of the line.

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
built-in function bound in the initial scope.  The initial scope has
- `_` ignores its argument and returns an empty list.
- `0` returns its argument with 0 prepended.
- `1` returns its argument with 1 prepended.

Examples
========

Hello world
-----------
```
0 0 0 1 0 0 1 0  1 0 1 0 0 1 1 0  0 0 1 1 0 1 1 0  0 0 1 1 0 1 1 0
1 1 1 1 0 1 1 0  0 0 0 0 0 1 0 0  1 1 1 0 1 1 1 0  1 1 1 1 0 1 1 0
0 1 0 0 1 1 1 0  0 0 1 1 0 1 1 0  0 0 1 0 0 1 1 0  1 0 0 0 0 1 0 0
0 1 0 1 0 0 0 0  _.
```

Cat
---
```
(_,0,1).
```

Tape of bits
------------
A finite tape of bits can be encoded as pairs of bits, where the first
bit is a non-nil flag and the second bit is the bit value, where the
first pair is the bit at the head, the second, fourth, sixth, etc pairs
are the bits to the right and the third, fifth, seventh, etc pairs are
the bits to the left.
```
LET
  make-tape = LET make-tape* = (_,1 0 0 0 make-tape*,1 1 0 0 make-tape*)
              IN  (_,1 0 make-tape*,1 1 make-tape*),
  from-tape = LET from-tape* = (_,(bit,from-tape* cdr cdr),_) (nil?,(_,0,1))
              IN  (bit,from-tape* cdr),

  bit = (_,_,(_,0 _,1 _)),
  nil? = (1 _,1 _,0 _),
  set-nil = (_,0,0),
  set-1 = (1 1,(1 1,1 1,1 1),(1 1,1 1,1 1)),
  set-0 = (1 0,(1 0,1 0,1 0),(1 0,1 0,1 0)),

  car = (0 0 _,(0 0 _,0 0 _,0 0 _),(0 0 _,1 0 _,1 1 _)),
  cdr = (_,(_,(_,0,1),(_,0,1)),(_,(_,0,1),(_,0,1))),
  b[0] = car,
  b[1] = car cdr,
  b[2] = car cdr cdr,
  b[3] = car cdr cdr cdr,

  > = LET >* = (_,(b[3],(b[0],>* cdr cdr)) 0,(b[3],(b[0],>* cdr cdr)) 1)
      IN  (b[1],(b[3],(b[0],>* cdr cdr))),
  < = LET <* = (_,(b[3],(b[0],<* cdr cdr)) 0,(b[3],(b[0],<* cdr cdr)) 1)
      IN  (b[2],(b[0],<* cdr)),

  rewind = (_,rewind <,(_,0,1)) (nil? b[2],(_,0,1))
IN
  etc.

```
Multiple tapes can be encoded by interleaving the bits of each tape and
defining operations that read, write, and move the head for each tape
independently.

A brainfuck interpreter could be implemented in Pointless by encoding tapes
for input, output, code, and data as its state.

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
