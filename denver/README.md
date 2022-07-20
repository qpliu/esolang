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

  statement = assignment-statement | break-statement |
              continue-statement | loop-statement | message-statement
              | guard* statement
  assignment-statement = variable-identifier '<' (expression | spawn)
  break-statement = loop-identifier? 'break'
  continue-statement = loop-identifier? 'continue'
  loop-statement = loop-identifier? body
  message-statement = '[' message-statement-arm* ']'

  expression = variable-identifier | 'null' | 'self'
  guard = expression ('=' | '!') expression
  spawn = '[' routine-identifier expression* ']'

  message-statement-arm = loop-identifier? (receive | send)
  receive = variable-identifier variable-identifier '<' expression* body
  send = expression '<' expression body

  loop-identifier = identifier
  thread-identifier = identifier
  variable-identifier = identifier
```

Comments begin with `==` and extend to the end of the line.

Identifiers are uninterrupted sequences of non-space characters,
excluding `=`, `!`, `[`, `]`, `{`, `}`, `<`, and may not be
`break`, `continue`, `null`, or `self`.

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

If a guard is `=`, then the rest of the statement is only executed if
the two expressions evaluate to the same thread.

If a guard is `!`, then the rest of the statement is only executed if
the two expression evaluate to different threads.

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
active.  An active arm attempts to send to a thread, receive from a
thread, or receive from any thread.  If there are no active arms,
execution continues to the next statement.  If there are multiple
active arms, execution is blocked until one or more active arms can
succeed.  One of the arms that can succeed will succeed, and its body
will be executed, then, unless the body exits the message statement,
the message statement is executed again.

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

Sending `self` to the system thread attempts to lock the system thread
for the sender.  If another thread has the lock, the system thread
sends the `null` thread to the sender.  If no thread has the lock or
if the sender already has the lock, the sender gets the lock and the
system thread resets its results list and sends the first item in its
results list to the sender.

Sending `null` to the system thread releases the lock on the system
thread if the sender has the lock.  If the sender does not have the
lock, there is no effect.

Sending the system thread to the system thread requests the next item
in the results list.  If the sender does not have the lock, the system
thread sends `null` to the sender.  If the sender does have the lock,
the first item in the results list is dropped from the results list
and the system thread sends the next item to the sender.  If the
results list is empty, the system thread sends the `null` thread to
the sender.

Sending anything else to the system thread causes the system thread to
send `null` to the sending thread.

When reset, results list contains the system thread, the input thread,
and the output thread.

### Input thread
When anything is sent to the input thread, if there is no more input,
the input thread sends `null` to the sender, otherwise, if the next
bit of input is 1, the input thread sends the input thread to the
sender, otherwise, the bit of input is 0 and the input thread sends
the sender to the sender.

There is no guaranteed order if multiple threads send to the input
thread.

### Output thread
The output thread never sends any messages.  If it receives `null`, a
0 bit is output.  If it receives anything else, a 1 bit is output.

There is no guaranteed order if multiple threads send to the output
thread.

Examples
--------
### cat
```
main system {
  [resp=null system < self { [resp < system {break}] }]
  [system < system { break }]
  [in _ < system { break }]
  [system < system { break }]
  [out _ < system { break }]
  [system < null { break }]

  state < self
  == state=self: receiving, state=null: sending 0, state=in: sending 1
  [ state=self state _ < in {
      state=null break
      state!in state < null
    }
    state!self out < state {
      state < self
    }
  ]
  break
}
```
### lock
```
lock locker {
  [ msg sender < {
      [ msg=self sender < locker { break } == query lock
        msg=sender locker=null sender < sender { locker < sender break } == acquire lock
        msg=sender locker!null sender < locker { break } == fail to acquire lock
        msg=null locker=sender sender < null { locker < null break } == release lock
        msg=null locker!sender sender < locker { break }
        msg!self msg!sender msg!null sender < locker { break }
      ]
    }
  ]
}
```
