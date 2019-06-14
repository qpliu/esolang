Beets
=====
Initial notes for the Beets programming language.

Data Type
---------
The data type is an infinite binary tree of bits.

Grammar
-------
```
  program = definition*
  definition = identifier identifier* '=' expression '.'
  expression = '(' expression ')' | parameter | call | tree | left-subtree | right-subtree | conditional
  parameter = identifier
  call = identifier expression*
  tree = ('0' | '1') expression ',' expression
  left-subtree = '<' expression
  right-subtree = '>' expression
  conditional = expression '?' expression ',' expression
```

Comments start with `==` and extend to the end of the line.

An identifier is an uninterrupted sequence of non-space characters,
excluding `=`, `.`, `,`, `<`, `>`, `?`, `(`, `)`, and may not begin
with `0` or `1`.

Space characters separate identifiers and are otherwise ignored.

Expressions
-----------
### Tree
Creates a new tree from a root bit, and two subtrees.
```
  tree = ('0' | '1') expression ',' expression
```
For example, this tree has `1` at its head, with `x` as its left
subtree and `y` as its right subtree.
```
  1x,y
```
### Left subtree
The left subtree of a tree.
```
  left-subtree = '<' expression
```
For example, this tree is the left subtree of `t`.
```
  <t
```
### Right subtree
The right subtree of a tree.
```
  right-subtree = '>' expression
```
For example, this tree is the right subtree of `t`.
```
  >t
```
### Conditional
The conditional expression evaluates to the left expression if the
root bit of the condition evaluates to `0`, otherwise it evaluates to
the right expression.
```
  conditional = expression '?' expression ',' expression
```
For example, this evaluates to `b`.
```
  (1x,y)?a,b
```
Note that without the parentheses, it is not specified whether the
condition is `y` or `1x,y`.  This document leaves the interpretation
of such ambiguous expressions to be defined by the implementation.
To avoid ambiguity, write `(>a)?b,c` or `>(a?b,c)` instead of writing
`>a?b,c`.

Examples
--------

### cat
```
  cat x = x.
```
### hello-world (for one possible encoding scheme)
```
  z = 0z,z.
  o rest = 10rest,z,z.
  i rest = 11rest,z,z.
  hello-world = o o o i o o i o i o i o o i i o o o i i o i i o o o i
                i o i i o i i i i o i i o o o o o o i o o i i i o i i
                i o i i i i o i i o o i o o i i i o o o i i o i i o o
                o i o o i i o i o o o o i o o o i o i o o o o z.
```
### reverse
```
  reverse t = t?0reverse>t,reverse<t,1reverse>t,reverse<t.
```
### invert
```
  invert t = t?1invert<t,invert>t,0invert<t,invert>t.
```
