Tower of Annoy
==============
Initial notes for the Tower of Annoy programming language.

Data Type
---------
The data-type is tower which is a stack of towers.  A tower has a
size.  An empty tower has size 1.  The size of a tower is the sum of
the sizes of the towers in it plus 1.  When tower a is pushed onto
tower b, any towers contained by b that have size less than the size
of a are destroyed.  Thus, the stack always has the largest tower on
the bottom and the smallest on the top.

Note that towers with the same size are not necessarily equal, but the
language only allows direct comparison of sizes.  A test of structural
equality is possible with a recursive function that deconstructs and
reconstructs the values.

A value that is pushed onto another value is not directly accessible.
An implementation must fail with a compile-time or a run-time error if
an inacessible value is accessed.  An inaccessible value becomes
accessible once it is popped.  An inaccessible value that is destroyed
due to a larger sized value being pushed on top of it will forever
remain inaccessible.

Grammar
-------
```
  program = statements? '.'?
  statements = statement ('.' statement)*
  statement = assignment | define-function | block | expr
  assignment = identifier ':=' expr
  define-function = identifier '(' (identifier ( ',' identifier)*)? ')'
      ':=' (block | identifier)
  block = '{' (statements '.')? 'return'? expr '}'
  expr = '0' | identifier | push-expr | pop-expr | gt-expr | lt-expr |
      eq-expr | call-function | '(' expr ')'
  push-expr = expr '+' expr block?
  pop-expr = expr '-' (identifier block)?
  gt-expr = expr '>' expr block?
  lt-expr = expr '<' expr block?
  eq-expr = expr '=' expr block?
  call-function = identifier '(' (expr (',' expr)*)? ')'
```

Comments start with `//` and extend to the end of the line.

Identifiers are sequences of alphanumeric or `_` characters, or
sequences of any character between double quotes, with backslash
escaping for backslash or double quote characters.  The only reserved
words are `return` and `0`, and identifiers named `return` or `0` are
possible when using double quotes, as `"return"` or `"0"`.

Whitespace may be used between tokens and is otherwise ignored.
Whitespace must be used to separate the `return` token and an
identifier that is not in double quotes or `0`.

Expressions
-----------
push-expr: `a+b` evaluates to `a` with `b` pushed on top, crushing
anything < `b`.  `a+b{block}`, if `b` can be pushed on top of `a`
without crushing anything, evaluates to `a` with `b` pushed on top,
otherwise, `b` is not pushed onto `a` and evaluates to `block`.

pop-expr: `a-` evaluates to `a` with the top of `a` popped.  if `a` is
empty, then `a-` evaluates to `a`.  `a-b{block}` evaluates to `block`
with top of `a` popped and with the popped value assigned to `b`.  the
scope of `b` is limited to `block`.  If `a` is empty, `a-b{block}`
evaluates to `a`.

gt-expr: `a>b` with no block evaluates to `a` if `a`>`b`, else `b`.
`a>b{block}` evaluates to block if `a`>`b`, else `b`.

lt-expr: `a<b` with no block evaluates to `a` if `a`<`b`, else `b`.
`a<b{block}` evaluates to `block` if `a`<`b`, else `b`.

eq-expr: `a=b` with no block evaluates to `a` if `a=b`, else `b`.
`a=b{block}` evaluates to `block` if `a=b`, else `b`.

Scoping
-------
The scope of functions and identifiers defined within a block is
limited to the block.  Every assignment statement defines a new
identifier, shadowing any previous identifier with the same name,
making Tower of Annoy a single-assignment language.

It should be able to statically determine if a value referred to by an
identifier can possibly have been pushed, and it should be illegal to
use that identifier in an expr after it can have been pushed.

The return statement returns from the containing function.  A return
statement not in a function causes the program to terminate.

Library Functions
-----------------
To use a library function, define a function with a name.
```
  f() := name_of_library_function.
```
Library functions are implementation defined.

A library function may be overloaded with respect to the number of
arguments, but a declaration with an inappropriate number of arguments
should be an error.

Input/Output
------------
Input/output may be available as implementation defined library
functions.

Examples
--------
cat:
```
  cat() := {
    // read and write are library functions
    read() := "read". // returns 0 for EOF, otherwise the top of the result is the next value read
    write(a) := "write".
    read()-a {
      write(a).
      cat()
    }
  }.
```
structural equality:
```
  // return 0 if equal, 0+0 if not
  eq(a,b) := {
    z := 0.
    a=b {
      a=z {
        return z
      }.
      a-aa {
        b-bb {
          eq(aa,bb)=z {
            eq(a,b)=z {
              a+aa.
              b+bb.
              return z
            }
          }.
          a+aa.
          b+bb
        }
      }
    }.
    z+0
  }.
```
Implementation Notes
--------------------
Should have tail call elimination.

A library function needs to indicate which arguments can have been
pushed when the function returns.

A library function needs to indicate which arguments can be possibly
be the return value.

When determining a lifetime of an identifier, need to consider
aliasing.  If an identifier is aliased, and the alias is pushed, it
should be illegal to refer to the identifier as well as the alias
after the alias is pushed.

Since no references are allowed to pushed values, cyclical references
are impossible, so reference counting is sufficient for garbage
collection, and all values recursively contained in a garbage
collected value can be garbage collected.

Should be aware that function arguments could alias each other as well
as well as the values of other identifiers in the scope that encloses
the function, so it's probably too difficult to statically determine
the lifetimes of identifiers.

It might have to be a run-time error to refer to a value that has been
pushed.  But then, what if an identifier refers to a value that then
gets pushed and then gets popped?  That can be solved by using a
version number that is incremented when pushed, so if an identifier's
version number does not match the value's version number, it's
invalid.

Still, want to implement as much static analysis as possible to flag
anything that is statically determined to refer to values that have
been pushed errors.
