Neck Sheen
==========
Neck Sheen is a programming language that features concurrency and
message passing.

Neck Sheen programs can do calculations on bits by combining nand
expressions and can fork new threads with which it can send and
receive bits over a queue associated with the thread.

Neck Sheen variables are static single-assignment.  The
previous-variable expression, written as `variable < expression`,
serves as the Φ function in loops.

Neck Sheen takes ideas from its precursor, [Neck Sheen
(precursor)](precursor/README.md), replacing the thread topology with
a dynamic tree.

Grammar
-------
```
  program = statement*

  statement = assignment | break | continue | fork | loop | receive | send
  assignment = variable '=' expression '.'
  break = loop-identifier? 'break' expression? '.'
  continue = loop-identifier? 'continue' expression? '.'
  fork = queue '+' (queue '.' | '{' statement* '}')
  loop = loop-identifier? '{' statement* '}'
  receive = queue '>' variable loop-identifier? '.'
  send = queue '<' expression ('.' | '{' statement* '}')

  expression = variable | previous-variable | nand | '(' expression ')'
  previous-variable = variable '<' expression
  nand = expression expression

  loop-identifier = identifier
  queue = identifier
  variable = identifier
```

Comments begin with `==` and extend to the end of the line.

Identifiers are uninterrupted sequences of non-space characters,
excluding `=`, `.`, `(`, `)`, `{`, `}`, `<`, `>`, and may not be
`break` or `continue`.

There are two identifier name spaces: one for loops and queues, and
one for variables.

Loop identifiers are declared with loop statements and are optionally
referenced by break and continue statements.  The scope of a loop
identifier is its body, excluding any fork statements.  The loop
identifier must not duplicate any loop identifier or queue identifier
that is in scope.

Queue identifiers are declared with fork statements and are referenced
by fork, receive, and send statements.  The scope a queue identifier
starts with the declaration and ends at the end of the innermost
enclosing loop statement, excluding the bodies of other fork
statements.  Queue identifiers must not duplicate any queue identifier
that is in scope or any loop identifier that is in scope.  There is
one predefined queue, `io`, which can be used to receive input and
send output.  The queue identifier is also the loop identifier of the
body of the fork statement.

Variable identifiers are declared with assignment, receive, or test
statements.  The scope of the variable starts with the statement
following the declaration and ends at the end of the innermost
enclosing loop statement.  A variable also has a pre-scope, which
starts with the start of the innermost enclosing loop statement and
ends with the declaration.  Variable identifiers must not duplicate
any variable identifier that is in scope.  There is one predefined
variable, `0`, which evaluates to false.  Expressions may only
reference variables that are in scope, except for previous-variable
expressions, which may be either in the variable's scope or the
variable's pre-scope.

Statements
----------
The statements of a program form an implicit unnamed loop.

An assignment statement declares and assigns the (single bit) value of
the expression to the variable.  A variable may not be reassigned.

A break statement exits the named or, if unspecified, the innermost
enclosing, loop statement if the expression evaluates to true or if
the expression is omitted.  If the loop is exited, all queues declared
in the loop are closed.

A continue statement returns execution to the top of the named or, if
unspecified, the innermost enclosing, loop statement if the expression
evaluates to true or if the expression is omitted.  If the loop is
continued, all queues declared in the loop are closed.

A fork statement declares a queue with either a loop or another queue.
Executing a fork starts a new thread.  The forking thread can
communicate with the new thread by receiving from or sending to the
queue.  The new thread executes the body of the fork statement as a
loop and can communicate with the forking thread by receiving from or
sending to the queue.  If the fork statement references another queue
instead of specifying a loop, the other queue must be declared with a
loop and the other queue identifier must be in scope, and the new
thread communicated with the forking thread with the other queue
identifier, while the forking thread communicates with the new thread
with the queue identifier of the fork statement.  The new thread exits
when it breaks the loop that is the body of the fork statement, and
the queue identifier is also the loop identifier.  The values and
previous values of variables that are declared outside of the fork
statement and whose scope includes the fork statement are frozen at
the time the fork statement is executed for the forked thread.

A loop statement is an optionally named list of statements that are
executed in sequence.  When execution reaches the end of the list of
statements, all queues declared in the loop are closed and execution
resumes at the start of the list of statements.

A receive statement declares a variable, which is set to the bit
received from the queue.  If the queue is closed for receiving, the
named, or if unspecified, the innermost enclosing, loop statement is
exited.

A send statement evaluates the expression and sends the result to the
queue.  If the queue is closed for sending and the loop is specified,
the loop is executed.  The loop is unnamed, so if a named `break` or a
named `continue` is needed, a nested named loop should be used.  The
loop cannot have the name of the queue when in the body of a fork
statement that declares the queue, since the loop in the fork has that
name.  (Due to a flaw in the implementation, if an empty loop is
specified, the implementation does not execute the (infinite) loop if
the queue is closed for sending.)

Expressions
-----------
A variable expression evaluates to the value assigned to the variable
in its declaration.  The expression must be in the scope of the
variable.

A previous-variable expression evaluates to the value last assigned to
the variable in a previous iteration of the loop in which it is
declared.  If executing the first iteration of the loop or if the
variable declaration was not executed in any previous iteration, the
previous-variable expression evaluates to its subexpression (to the
right of the `<`).  The expression must be in either the scope or the
pre-scope of the variable.  The predefined variable `0` has no
previous value, so previous-variable expressions with `0` always
evaluate to their subexpressions.

A nand expression is two expressions, which evaluates to the nand of
its subexpressions.  The nand operation is left-associative.

Parentheses can be used to specify the associativity of the nand
expressions or to limit the extent of previous-variable expressions.

Queues
------
A fork statement declares a queue that can be used to send and receive
bits between the forking thread and the new thread.  A queue is
initially open. The scope of the queue in the forking thread are the
statements in the innermost enclosing loop that follow the fork
statement.  The scope of the queue in the new thread is the body of
the fork statement.

All queues declared in a loop are closed when the loop iterates
(either by a continue statement or by reaching the end of the loop) or
when the loop exits.  A closed queue is closed for both sending and
receiving.

The predefined `io` queue is used for input and output.  The queue is
open for receiving as long as there is pending input.  After the input
has been completely received, the `io` queue is closed for receiving.
The queue is always open for sending.  The scope of the `io` queue is
the program, excluding the bodies of any fork statements.  The `io`
queue has no fork body, and thus cannot be referenced by a fork
statement.

Examples
--------
cat
```
  io > bit.
  io < bit.
```

tac (reverses the bytes of input, partial bytes at the end are discarded)
```
  stack+{
    == input: push: true bit, pop: false ignored
    == output: push: ignore ignore, pop: not-empty bit-or-ignore-if-empty
    loop {
      stack > op. == true bit: push, false ignore: pop
      {
        break op.  == skip if push
        stack > _. == discard argument
	== pop (empty)
        stack < 0.
        stack < 0.
	loop continue.
      }
      stack > bit.
      stack < 0.
      stack < 0.
      push+stack.
      {
        top = new-top < bit.
        stack > op2.
        stack > arg.
        push < op2.
	push < top.
        push > next-not-empty.
	push > next-top.
        stack < 0 0.
        stack < top.

        loop continue 0 0 (0 0 op2 (0 0 next-not-empty)).
        == continue if not (op2 is true (push) or next-not-empty is true)

        not-arg-or-true = op2 arg.
        == if op2 is true then (not arg) otherwise true

        true-or-not-next-top = 0 0 op2 next-top.
        == if op2 is false then (not next-top) otherwise true

	new-top = not-arg-or-true true-or-not-next-top.
        == if op2 is false, new-top = next-top, if op2 is true, new-top = arg
      }
    }
  }
  {
    io > bit1. == breaks loop when io is closed for receiving on EOF
    io > bit2. == breaks loop when io is closed for receiving on EOF
    io > bit3. == breaks loop when io is closed for receiving on EOF
    io > bit4. == breaks loop when io is closed for receiving on EOF
    io > bit5. == breaks loop when io is closed for receiving on EOF
    io > bit6. == breaks loop when io is closed for receiving on EOF
    io > bit7. == breaks loop when io is closed for receiving on EOF
    io > bit8. == breaks loop when io is closed for receiving on EOF
    stack < 0 0. == push
    stack < bit1.
    stack > ignore1-1.
    stack > ignore2-1.
    stack < 0 0. == push
    stack < bit2.
    stack > ignore1-2.
    stack > ignore2-2.
    stack < 0 0. == push
    stack < bit3.
    stack > ignore1-3.
    stack > ignore2-3.
    stack < 0 0. == push
    stack < bit4.
    stack > ignore1-4.
    stack > ignore2-4.
    stack < 0 0. == push
    stack < bit5.
    stack > ignore1-5.
    stack > ignore2-5.
    stack < 0 0. == push
    stack < bit6.
    stack > ignore1-6.
    stack > ignore2-6.
    stack < 0 0. == push
    stack < bit7.
    stack > ignore1-7.
    stack > ignore2-7.
    stack < 0 0. == push
    stack < bit8.
    stack > ignore1-8.
    stack > ignore2-8.
  }
  {
    stack < 0. == pop
    stack < 0. == ignored
    stack > not-empty8.
    break 0 0 not-empty8.
    stack > bit8.
    stack < 0. == pop
    stack < 0. == ignored
    stack > not-empty7.
    break 0 0 not-empty7.
    stack > bit7.
    stack < 0. == pop
    stack < 0. == ignored
    stack > not-empty6.
    break 0 0 not-empty6.
    stack > bit6.
    stack < 0. == pop
    stack < 0. == ignored
    stack > not-empty5.
    break 0 0 not-empty5.
    stack > bit5.
    stack < 0. == pop
    stack < 0. == ignored
    stack > not-empty4.
    break 0 0 not-empty4.
    stack > bit4.
    stack < 0. == pop
    stack < 0. == ignored
    stack > not-empty3.
    break 0 0 not-empty3.
    stack > bit3.
    stack < 0. == pop
    stack < 0. == ignored
    stack > not-empty2.
    break 0 0 not-empty2.
    stack > bit2.
    stack < 0. == pop
    stack < 0. == ignored
    stack > not-empty1.
    break 0 0 not-empty1.
    stack > bit1.
    io < bit1.
    io < bit2.
    io < bit3.
    io < bit4.
    io < bit5.
    io < bit6.
    io < bit7.
    io < bit8.
  }
  break.
```
