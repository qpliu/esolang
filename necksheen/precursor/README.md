Neck Sheen (precursor)
==========
Neck Sheen (precursor) is a precursor to the [Neck Sheen](../README.md)
programming language, and is a programming language featuring concurrency
and message passing.

A Neck Sheen (precursor) program takes N bits of input, then starts N+1 threads
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
  == reverses the bytes of the input
  io > if-not-head.
  if-head = 0 0 if-not-head.
  {
    break if-not-head.

    == mark the start of bytes
    right < 0 0.

    {
      left > x.
      break x.
    }

    == start the last byte
    left < 0 0.
    == this assumes the number of bits in the input is a multiple of 8
    == if not this will deadlock

    == wait for the first byte to finish
    right > _.
    break 0 0.
  }

  body {
    break if-head.
    io > input.

    == find out which bit
    left > bit0.
    {
      break 0 0 bit0.
      == next bit is bit1
      right < 0.
      right < 0 0.

      == wait for start output signal
      right > _start.

      io < input.

      == signal to output rest of the byte
      right < 0.

      == wait for the byte to be finished
      right > _done.

      == signal to start the next byte
      left < 0.

      body break 0 0.
    }

    left > bit1.
    {
      break 0 0 bit1.
      == next bit is bit 2
      right < 0.
      right < 0.
      right < 0 0.

      == wait for start output signal
      right > _start.

      == relay start output signal to first bit
      left < 0.

      == wait for output bit signal
      left > _output.

      io < input.

      == signal to output rest of the byte
      right < 0.

      == wait for the byte to be finished
      right > _done.

      == signal to start the next byte
      left < 0.

      body break 0 0.
    }

    left > bit2.
    {
      break 0 0 bit2.
      == next bit is bit 3
      right < 0.
      right < 0.
      right < 0.
      right < 0 0.

      == wait for start output signal
      right > _start.

      == relay start output signal to first bit
      left < 0.

      == wait for output bit signal
      left > _output.

      io < input.

      == signal to output rest of the byte
      right < 0.

      == wait for the byte to be finished
      right > _done.

      == signal to start the next byte
      left < 0.

      body break 0 0.
    }

    left > bit3.
    {
      break 0 0 bit3.
      == next bit is bit 4
      right < 0.
      right < 0.
      right < 0.
      right < 0.
      right < 0 0.

      == wait for start output signal
      right > _start.

      == relay start output signal to first bit
      left < 0.

      == wait for output bit signal
      left > _output.

      io < input.

      == signal to output rest of the byte
      right < 0.

      == wait for the byte to be finished
      right > _done.

      == signal to start the next byte
      left < 0.

      body break 0 0.
    }

    left > bit4.
    {
      break 0 0 bit4.
      == next bit is bit 5
      right < 0.
      right < 0.
      right < 0.
      right < 0.
      right < 0.
      right < 0 0.

      == wait for start output signal
      right > _start.

      == relay start output signal to first bit
      left < 0.

      == wait for output bit signal
      left > _output.

      io < input.

      == signal to output rest of the byte
      right < 0.

      == wait for the byte to be finished
      right > _done.

      == signal to start the next byte
      left < 0.

      body break 0 0.
    }

    left > bit5.
    {
      break 0 0 bit5.
      == next bit is bit 6
      right < 0.
      right < 0.
      right < 0.
      right < 0.
      right < 0.
      right < 0.
      right < 0 0.

      == wait for start output signal
      right > _start.

      == relay start output signal to first bit
      left < 0.

      == wait for output bit signal
      left > _output.

      io < input.

      == signal to output rest of the byte
      right < 0.

      == wait for the byte to be finished
      right > _done.

      == signal to start the next byte
      left < 0.

      body break 0 0.
    }

    left > bit6.
    {
      break 0 0 bit6.
      == next bit is bit 7
      right < 0.
      right < 0.
      right < 0.
      right < 0.
      right < 0.
      right < 0.
      right < 0.
      right < 0 0.

      == wait for start output signal
      right > _start.

      == relay start output signal to first bit
      left < 0.

      == wait for output bit signal
      left > _output.

      io < input.

      == signal to output rest of the byte
      right < 0.

      == wait for the byte to be finished
      right > _done.

      == signal to start the next byte
      left < 0.

      body break 0 0.
    }

    left > _bit7.
    {
      == next bit is bit 0
      right < 0 0.

      == wait for start output signal
      right > _start.

      == relay start output signal to first bit
      left < 0.

      == wait for output bit signal
      left > _output.

      io < input.

      == signal to start the next byte
      left < 0.

      body break 0 0.
    }
  }
  break 0 0.
```
