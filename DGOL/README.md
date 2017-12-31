The DGOL Programming Language
=============================
DGOL (Directed Graph Oriented Language) is a programming language based on
manipulating nodes in directed graphs.

Syntax
------
DGOL's syntax is inspired by the syntax of FORTRAN 77 and Commodore BASIC.

All linear whitespace is ignored.  Comments begin with `*` and end at the
end of the line.  Lines with only whitespace or comments are ignored.

Each line begins with a keyword, followed by zero or more tokens.  A token
is either a string of 1 or more alphanumeric characters, or one of `=`,
`<`, `>`, `(`, `)`, or `.`.

Keywords are `USE`, `SUBROUTINE`, `END`, `LIBRARY`, `PROGRAM`, `LET`,
`IF`, `ELSEIF`, `ELSE`, `ENDIF`, `CALL`, `RETURN`, `DO`, and `EXIT`.

A value token is a token of 1 or more alphanumeric characters.

An identifier token a value token that is not `0`.

## Grammar
```ebnf
module = [ uses ] , [ subroutines ] , ( library | program ) ;

uses = "USE" , identifier , newline ;
subroutine = "SUBROUTINE" , identifier , parameterlist , newline ,
    { statement } ,
    "END" , identifier , newline ;
library = "LIBRARY" , identifier , newline ,
    export , { export } ,
    "END" , identifier , newline ;
program = "PROGRAM" , identifier , newline ,
    { statement } ,
    "END" , identifier , newline ;

export = "SUBROUTINE" , identifier , newline ;
statement = letequal | letaddedge | letremoveedge | if | call | return
    | do | exit ;

letequal = "LET" , identifier , "=" , value , newline ;
letaddedge = "LET" , identifier , ">" , value , newline ;
letremoveedge = "LET" , identifier , "<" , identifier , newline ;
if = "IF" , condition , newline ,
    { statement } ,
    { "ELSIF" , condition , newline , { statement } } ,
    [ "ELSE" , newline , { statement } ] ,
    "ENDIF" , newline ;
call = "CALL" , [ identifier , "." ] , identifier , argumentlist , newline ;
return = "RETURN" , newline ;
do = "DO" , identifier , [ "<" , identifier ] , newline ,
    { statement } ,
    "ENDDO" , newline ;
exit = "EXIT" , identifier , newline ;

parameterlist = "(" , [ identifier , { "," , identifier } ] , ")" ;
argumentlist = "(" , [ value , { "," , value } ] , ")" ;
condition = identifier , ( '=' | '>' ) , identifier ;

value = alphanum , { alphanum } ;
identifier = value - "0" ;

alphanum = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K"
    | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W"
    | "X" | "Y" | "Z" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8"
    | "9" ; (* may include any other alphanumeric characters *)
newline = "\n" ;
```

Modules
-------
A DGOL program is made up of exactly one program module and zero or more
library modules.

A module consists of zero or more `USE` declarations, zero or more
`SUBROUTINE` definitions, and either a `PROGRAM` definition or a
`LIBRARY` definition.

A `USE` declaration consists of the name of a library module.  Duplicate
`USE` declarations are not allowed within a module.

A library module must have at least one `SUBROUTINE` definition.

The names of the subroutines in the `SUBROUTINE` definitions must be
unique within the module.

## Library Modules
A library module defines one or more subroutines and exports one or more
subroutines.

The `LIBRARY` definition consists of the name of the module, and one or
more `SUBROUTINE` declarations.  The `LIBRARY` definition ends with an
`END` statement, which must consist of the name of the module.

A `SUBROUTINE` declaration consists of the name of the subroutine to be
exported, which must be defined in the module.

## Program Modules
A program module defines zero or more subroutines and the main program
routine that is initially called when the program is executed.

Routines
--------
A routine definition consists of the routine name, zero or more parameters,
and zero or more statements.  The routine definition ends with an `END`
statement, which must consist of the name of the routine.

The names of the parameters must be unique within the routine.

When a routine is called, the statements are executed sequentially.  If
the end of statements is reached, the routine returns.

## Subroutines
A `SUBROUTINE` definition is a routine definition.

A subroutine may be called with any number of arguments.  If there are more
arguments than parameters, the excess arguments are discared.  If there are
fewer arguments than parameters, the excess parameters are assigned with
new nodes.

## Program routines
A `PROGRAM` definition is a routine definition with no parameters and
may not contain any `RETURN` statements.

Variables and Nodes
-------------------
All variables refer to nodes.

A node consists of an identity and a set of edges to other nodes (or to
itself).

A value is either a node referenced by a variable, or a new node.  A new
node is written as `0`.

Subroutine call arguments are passed by reference, so `LET` assignments to
subroutine parameters affect the caller variables.  All other variables
initially contain new nodes.

Unreferenced nodes, those not referenced by a variable, a `DO` loop, nor in
a subgraph reachable from a node referenced by a variable or a `DO` loop,
should be garbage collected.

Statements
----------
## `LET` statements
There are three types of `LET` statements.
1. `LET` *var* `=` *val*

   Assigns the variable, *var*, to refer to the value, *val*.
2. `LET` *var* `>` *val*

   Adds an edge to the node referenced by the variable, *var*, to the
   value, *val*.  If there is already such an edge, nothing is done.
3. `LET` *var* `<` *var2*

   Removes any edge from the node referenced by the variable, *var*,
   to the node referenced by the variable, *var2*.  If there is no
   such edge, nothing is done.

## `IF` statements
1. `IF` *condition*  
   *statements*  
   *else-branches*  
   `ENDIF`

   There are two type of conditions.
   1. *var* `=` *var2*

      The condition is true if *var* and *var2* reference the same node.
   2. *var* `>` *var2*

      The condition is true if there is an edge from the node referenced
      by *var* to the node referenced by *var2*.

   The condition is evaluated, and if the condition is true, *statements*
   are executed and any *else-branches* are skipped.

   If the condition is not true, the next branch of *else-branches*,
   if any, is executed.

## `CALL` statements
There are two types of `CALL` statements.
1. `CALL` *routine* `(` *arguments* `)`

   Calls the subroutine, *routine*, defined in this module.
2. `CALL` *module* `.` *routine* `(` *arguments* `)`

   Calls the subroutine, *routine*, exported by the module, *module*, which
   must be declared with a `USE` declaration in this module.

Any number of arguments may be passed to the routine.  An argument can
be a variable or `0`.  A variable is passed by reference.  A `0` argument
is a new node.

## `RETURN` statements
1. `RETURN`

   A `RETURN` statement returns execution to the subroutine's caller.

   `RETURN` statements are not allowed in a program routine.

## `DO` statements
There are two types of `DO` statements.
1. `DO` *var*  
   *statements*  
   `ENDDO`

   Executes statements repeatedly in a loop forever unless exited
   by an `EXIT` or `RETURN` statement.
2. `DO` *var* `<` *var2*  
   statements  
   `ENDDO`

   Executes statements once for each edge of the node referenced by the
   variable, *var2*, in some unspecified order, where the variable, *var*,
   is assigned to reference the node pointed to by the edge at the
   beginning of each iteration, unless exited by an `EXIT` or `RETURN`
   statement.

   The set of nodes iterated over is set at the initial execution of the
   `DO` statement, and is not affected by changes to the edges of the
   node referenced by *var2* nor by any assignements to *var2* in the
   body of the `DO` statement.

## `EXIT` statements
1. `EXIT` *var*

   An `EXIT` statement exits the enclosing `DO` statement with the *var*,
   so that execution continues with the statement following the `DO`
   statement.

Standard Libraries
------------------
DGOL defines one standard library, `IO`, which exports two subroutines,
`READ BYTE` and `WRITE BYTE`.
```
* READ BYTE READS A BYTE FROM THE STANDARD INPUT.
* IF THE END OF FILE IS REACHED, AN EDGE IS ADDED FROM BYTE TO EOF.
* OTHERWISE, EDGES ARE ADDED FROM BYTE TO 1, 2, 4, 8, 10, 20, 40, AND 80
* FOR EACH CORRESPONDING 1 BIT IN THE BYTE READ.
SUBROUTINE READ BYTE(BYTE,EOF,1,2,4,8,10,20,40,80)
* IMPLEMENTATION NOT SPECIFIED
END READ BYTE

* WRITE BYTE WRITES A BYTE TO THE STANDARD OUTPUT.
* THE BYTE HAS 1 BITS CORRESPONDING TO EDGES FROM BYTE to 1, 2, 4, 8,
* 10, 20, 40, AND 80.
SUBROUTINE WRITE BYTE(BYTE,1,2,4,8,10,20,40,80)
* IMPLEMENTATION NOT SPECIFIED
END WRITE BYTE

LIBRARY IO
  SUBROUTINE READ BYTE
  SUBROUTINE WRITE BYTE
END IO
```

See also
--------
## Example Code
* [HELLO.DGOL](example/HELLO.DGOL)
* [CAT.DGOL](example/CAT.DGOL)
* [CYCLIC.DGOL](example/CYCLIC.DGOL)
* [PATH.DGOL](example/PATH.DGOL)
* [Z.DGOL](example/Z.DGOL)
* [BRAINFUCK.DGOL](example/BRAINFUCK.DGOL)

## Implementations
* [Intepreter in Go](go)

  The first implementation of DGOL.  Garbage collection is done by the
  Go runtime garbage collector.

* [Intepreter in Haskell](hs/interp)

  Implemented with a copying garbage collector.  Garbage collection is
  done after each loop iteration and after returning from each subroutine
  call.

* [Interpreter in C](c)

  Implemented with a mark and sweep garbage collector.  Nodes are allocated
  into a linked list.  Garbage collection is done after each loop iteration
  and after returning from each subroutine call.

* [Interpreter in Rust](rs)

  Implemented with a mark and sweep garbage collector.  Nodes are allocated
  in pages, which are a linked list of arrays of 256 nodes.  Garbage
  collection is done when all pages are full.  A new page is allocated when
  the last page has less than 16 free nodes after garbage collection.

* [Compiler to LLVM 5](hs/compiler)

  Garbage collection scheme is the same as with the interpreter in Rust.

  _TODO_: Implement a compacting garbage collector that moves nodes and
  frees empty pages if there are multiple pages with 240 or more free
  nodes.