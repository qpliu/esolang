Neck Sheen
==========
Neck Sheen is a programming language featuring concurrency and message
passing.

A Neck Sheen program takes N bits of input, then starts N+1 threads
running the program.  Each thread is connected to two other threads
via bit queues, one to the left and one to the right.  All threads
except the first thread has one bit of input, where the thread to the
left has the previous bit and the thread to the right has the next
bit and the first thread is left of the thread with the the first bit
and right of the thread with the last bit.

The only synchronization mechanism is receiving a bit from a bit queue,
which blocks when the queue is empty.  The bit queue must have a capacity
of at least one bit.  If the bit queue is at capacity, sending a bit
will block.

Other possible variants could have other topologies or other input and
output models.

Grammar
-------
```
  program = statement*
  statement = (assignment | break | continue | loop | receive | send)
  assignment = identifier '=' expression '.'
  break = identifier? 'break' expression '.'
  continue = identifier? 'continue' expression '.'
  loop = identifier? '{' statement* '}'
  receive = identifier '>' identifier '.'
  send = identifier '<' expression '.'
  expression = identifier ('<' expression)? | expression expression | '(' expression ')'
```

Comments begin with '==' and extend to the end of the line.

Identifiers are uninterrupted sequences of non-space characters, excluding
'=', '.', '(', ')', '{', '}', '<', '>', and may not be 'break' or 'continue'.

There are 3 identifier name spaces: loops, queues, variables.

Loop identifiers are declared with loop statements and are optionally
referenced by break and continue statements.  Loop identifiers must not
duplicate any enclosing loop identifiers and must not be referenced outside
its body.

Queue identifiers are predefined by the thread topology and input and output
models.  New queue identifiers cannot be declared.  For the variant in this
specification, the predefined queue identifiers are 'left', 'right', and 'io'.

Variables identifiers include predefined variables and are also declared by
assignment statements or receive statements.  Identifiers must be unique
within its scope and may not be reassigned.  For the variant in this
specification, there is one predefined variable, '0', which evaluates to false.

Statements
----------
The statements of a program form an implicit unnamed loop.

An assignment statement assigns the (single bit) value of the expression to
the variable.  This declares the variable, whose scope extends to the end
of the innermost enclosing loop.

A break statement exits the named or, if unspecified, innermost enclosing,
loop statement if the expression evaluates to true.

A continue statement returns to the top of the named, or if unspecified,
innermost enclosing, loop statement if the expression evaluates to true.

A loop statement is an optionally named list of statements that are repeatedly
executed unless exited with a break statement.

A receive statement takes a queue identifier (to the left of the '<') and
a variable identifier (to the right of the '<') and receives and bit from the
queue and assigns that value to the variable.  This declares the variable,
whose scope extends to the end of its innermost enclosing loop.

A send statement takes a queue identifier (to the left of the '>') and
evaluates the expression and adds the resulting bit to the queue.

Expressions
-----------
An expression can be an variable identifier.  The expression must be in the
scope of the variable.  It evaluates to the value previously assigned or
received at its declaration.

An expression can be an identifier, followed by '<' and an initial expression.
The expression must either be in the scope of the identifier variable, or
in its pre-scope.  In the initial execution of the innermost loop enclosing the
declaration of the variable, the expression evaluates to the initial
expression (which follows the '<').  In subsequent executions of the loop,
the expression evaluates to the value assigned by the declaration of the
identifier in the previous execution of the loop.  The pre-scope of a variable
include all statements in the innermost loop containing the variable's
declaration that precede the declaration without any continue statements
for that loop between the statement and the declaration.  Such continue
statements are also not in the pre-scope.

An expression can be two expressions, which evaluates to the NAND of its
subexpressions.  This is left-associative.

Parentheses can be used to specify the associativity of the NAND expressions
or to limit the extent of initial expressions.

Queues
------
This is for the input and output model for the variant in this
specification.  For the first thread, receiving from the 'io' queue yields
false, then blocks.  For every other thread, receiving from the 'io' queue
yields true, then one bit of the input, then blocks.  Sending to the 'io'
queue sends the bit to the thread's output.  This specification does not
specify how to map the outputs of the threads to a single stream of bits
(e.g. stdout) or to dual streams of bits (e.g. stdout and stderr).

This is for the ring (or necklace) topology of the variant in this
specification.  The 'left' queue can be used to send and receive bits to
and from the thread to the left and the 'right' queue can be used to send
and receive bits to and from the thread to the right.

Examples
--------
cat
```
  io > if-not-head.
  if-head = 0 0 if-not-head.
  {
    break if-not-head.
    right < 0.
    left > _.
    break 0 0.
  }
  {
    break if-head.
    io > i.
    left > _.
    io < i.
    right < 0.
    break 0 0.
  }
  break 0 0.
```

tac
```
== Not implemented
```
