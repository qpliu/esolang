Denver-Augusta-Harrisburg
=========================
Denver-Augusta-Harrisburg is a concurrent-message-passing programming
language.

Execution of a Denver-Augusta-Harrisburg program starts with the main
thread and the system thread.  The main thread executes the `main`
routine and can spawn more threads.  A thread can send messages to and
receive messages from other threads, including itself.  A message
consists of a thread.  When receiving a message, the sender thread is
also received.  Execution of the program ends when the main thread
exits.

Grammar
-------
```
  program = routine-definition*

  routine-definition = routine-identifier variable-identifier* body
  body = '{' statement* '}'

  statement = guard* (assignment-statement | break-statement |
                      continue-statement | loop-statement | message-statement)
  assignment-statement = variable-identifier '<' (expression | spawn)
  break-statement = loop-identifier? 'break'
  continue-statement = loop-identifier? 'continue'
  loop-statement = loop-identifier? body
  message-statement = '[' message-statement-arm* ']'

  expression = variable-identifier | 'null' | 'self'
  guard = expression? ('=' | '!') expression
  spawn = '[' routine-identifier expression* ']'

  message-statement-arm = guard* (receive | send) body
  receive = variable-identifier variable-identifier '<' expression*
  send = expression '<' expression

  loop-identifier = identifier
  routine-identifier = identifier
  thread-identifier = identifier
  variable-identifier = identifier
```

Comments begin with `==` and extend to the end of the line.

Identifiers are uninterrupted sequences of non-space characters,
excluding `=`, `!`, `[`, `]`, `{`, `}`, `<`, and may not be
`break`, `continue`, `null`, or `self`.  Space characters serve to
separate tokens and are otherwise ignored.

Routines
--------
A routine is declared with its routine identifier, a list of
parameters, and a body.  The routine identifier is also the loop
identifier of the body.  The parameters must be distinct from each
other.

Routine identifiers must be unique within a program.

When a thread is spawned, it executes the routine it was spawned with
by executing the statements in its body in a loop.  The thread exits
when the routine body is exited.

Statements
----------
Every statement may have a guard list, where the statement is executed
only if each item in the guard list is satisfied.  A guard list may be
empty.

If a guard is `expression = expression`, then the rest of the
statement is only executed if the two expressions evaluate to the same
thread.

If a guard is `expression ! expression`, then the rest of the
statement is only executed if the two expression evaluate to different
threads.

If a guard is `= expression`, then the rest of the statement is only
executed if the expression evaluates to a thread that has not exited.
It is not guaranteed that that thread has not executed when the rest
of the statement executes.  The `null` thread is always considered to
have exited.

If a guard is `! expression`, then the rest of the statement is only
executed if the expression evaluates to tha thread that has exited.

### Assignment statement
An assignment statement sets the variable to the value of the
expression.  If the expression is a spawn expression, it spawns a new
thread and set the variable to the new thread.  The new thread
executes the routine specified in the spawn expression, with its
parameters set to the arguments in the spawn expression.

Other than spawn expressions, expressions are a variable, `null`, or
`self`.  A variable evaluates to the value the variable was last
assigned, either by an assignment statement or a receive arm of a message
statement, or the `null` thread if not previously assigned.  `null`
evaluates to the `null` thread.  `self` evaluates to the currently
executing thread.

### Break statement
A break statement exits the specified, or, if not specified, innermost
enclosing loop statement or message statement or routine body.

### Continue statement
A continue statement continues executing at the beginning of the
specified, or, if not specified, innermost enclosing loop statement or
message statement or routine body.

### Loop statement
A loop statement specifies a list of statements that are executed in a
loop until exited.  A loop can be exited by exit statement or by a
continuing an enclosing loop statement, message statement or routine
body.

### Message statement
A message statement specifies a list of arms.  An arm may have a guard
list, for which each item must be satisfied for the arm to be
active.  An active arm attempts to send to a thread, receive from any
in a list of threads, or receive from any thread.  If there are no
active arms, execution continues to the next statement.  If there are
multiple active arms, execution is blocked until one or more active
arms can succeed.  One of the arms that can succeed will succeed, and
its body will be executed, then, unless the body of the successful arm
exits the message statement, the message statement is executed again.
If execution is blocked because every active arm is attempting to send
to an exited thread or the `null` thread, or is attempting to receive
only from exited threads or the `null` thread, execution continues to
the next statement.

A send arm sends its right argument to its left argument.

A receive arm assigns the message received to the first variable on
the left and the sender to the second variable on the left.  The first
variable and the second variable must not be the same variable.  The
arguments on the right is a list of sender threads.  If the list is
not empty, it can only receive from a thread in the list.  If the list
is empty, it can receive from any thread.

Loop identifiers
----------------
A loop identifier is associated with a routine, a loop statement, or a
message statement.  `break` and `continue` statements may specify a
loop identifier to specify which loop statement or message statement
they apply to, or if they apply to the body of the routine.

The scope of the loop identifier is the body of its loop statement or
the bodies of every arm of its message statement.

The routine identifier is the loop identifier of the routine body and
its scope is the routine body.

A loop identifier must not duplicate any other loop identifier that is
in scope.

Variables
---------
All variables are scoped to the routine in which it is used and,
except for the routine parameters, are initially set to `null`.  The
routine parameters, listed after the routine identifier in the routine
declaration, are initially set to values specified as arguments in the
spawn expression.  If there are fewer arguments provided in the spawn
expression than the number of routine parameters, the remaining
parameters are set to `null`.

Threads
-------
A new thread is spawned when an assignment statement is executed with
a spawn expression.  The variable on the left-hand side of the
assignment statement is assigned the new thread.  The new thread
executes the routine specified by the routine identifier in the spawn
expression, and its parameters are set to the arguments specified in
the spawn expression.  If there are more parameters than arguments,
the remaining parameters are set to `null`.  If there are more
arguments than parameters, the remaining arguments are ignored.

There are also some special threads.

### Main thread
The main thread is spawned when the program starts and executes the
`main` routine.  If it has parameters, the first parameter is set to
the system thread.  Any additional parameters are set to the `null`
thread.

### Null thread
Uninitialized variables are set to the `null` thread, which never
sends messages and ignores any message it receives.

### System thread
The system thread is passed as the first parameter to the `main`
thread.

The system thread has a list of threads that provide various
services.  Sending any item in the list to the system thread causes
the system thread to send the next item in the list to the sender.
The first item in the list is the system thread itself.  Sending the
last element in the list or a thread not in the list to the system
thread causes the system thread to send the `null` thread to the
sender.

The list of threads contains the system thread, the input thread, and
the output thread.

### Input thread
When anything is sent to the input thread, if the next bit of input is
1, the input thread sends the input thread to the sender.  If the next
bit of input is 0, the input thread sends `null` to the sender.  If
there is no more input, the input thread exits.

There is no guaranteed ordering if multiple threads send to the input
thread.

### Output thread
The output thread never sends any messages.  If it receives `null`, a
0 bit is output.  If it receives anything else, a 1 bit is output.

There is no guaranteed ordering if multiple threads send to the output
thread.

Examples
--------
### cat
```
main system {
  [in=null  system < system {[in _ < system  {break}]}]
  [out=null system < in     {[out _ < system {break}]}]

  [state=null =in in < self {state < in}
   state=in   =in b _ < in  {state < out}
   state=out      out < b   {state < null}
  ]
  break
}
```
### lock
```
lock locker {
  == send lock to lock to query lock
  == send self to lock to acquire lock
  == send null to lock to release lock
  == lock always responds with lock holder or null if not held
  [ msg sender < {
      [ msg=self   sender < locker { break } == query lock
        msg=sender sender < sender { locker=null locker < sender break } == acquire lock
        msg=null locker=sender sender < null   { locker < null break } == release lock
        msg=null locker!sender sender < locker { break }
        msg!self msg!sender msg!null sender < locker { break }
      ]
    }
  ]
}
```
### list
```
cons car cdr {
  == send null to get car
  == send anything else to get cdr
  serve [
    op sender < {
      [op=null sender < car {serve continue}
       op!null sender < cdr {serve continue}
      ]
    }
  ]
}
```
### stack
```
stack nil {
  == send nil to stack to pop, empty stack returns nil
  == send anything else to push
  == not for multithreaded use
  list < null
  serve [
    msg sender < {
      msg=nil list=null [sender < nil {serve continue}]
      msg=nil [
        list < null {
          [car _ < list {
              [sender < car {break}]
              [list < list {[list _ < list {serve continue}]}]
            }
          ]
	}
      ]
      list < [cons msg list]
    }
  ]
}
```
### tape
```
exited{break}

tape ? + - {
  == send ? to read
  == send + to advance one element
  == send - to rewind one element
  == send anything else to write
  == unwritten elements default to null
  head < null
  nil < [exited] == use exited thread as a private constant
  fwd < [stack nil]
  bwd < [stack nil]
  serve [
    msg sender < {
      msg=? {[sender < head {serve continue}]}
      msg=+ {[bwd < head {[fwd < nil {[head _ < fwd {head=nil head < null serve continue}]}]}]}
      msg=- {[fwd < head {[bwd < nil {[head _ < bwd {head=nil head < null serve continue}]}]}]}
      msg!? msg!+ msg!- {head < msg}
    }
  ]
}
```
