PUKE is an implementation of the INTERCAL programming language
described by http://www.muppetlabs.com/~breadbox/intercal/intercal.txt
with the exception of using Unicode instead of EBCDIC.

COMPILAC is the PUKE INTERCAL interpreter.

INTERLAC is the PUKE INTERCAL compiler targeting LLVM.  It works with
version 3, version 5, version 6, and version 7.  It probably works with
version 4 as well.

It runs on macOS 10.13 with Go 1.9.  It probably works on other versions
and other platforms, but it's never been tried.

This implementation attempts to follow the 1973 document in
preference to any later changes or additions.  As the 1973 document
references the Atari Implementation, it probably includes revisions
from the 1980s.  It seems reasonable to me to assume that these
revisions are limited to the Notes On The Atari Implementation and
the ASCII entries in the character set table.

Please note that this implementation is very buggy.  Some of the bugs
might be worked around by inserting `DO NOTHING MORE THAN KILLING THE
BUGGY COMPILER` at various places in your program.

Notes
-----
In section 4.4.1 of the 1973 document says
  to invert the least significant bit of the first element of 16-bit
  2-dimensional array number 1, one could write:
```
    ,1SUB#1#1 <- ’∀,1SUB#1#1¢#1’~’#0¢#65535’
```
and the code in the 1996 revision was changed to
```
    ,1SUB#1#1 <- '?",1SUB#1#1"$#1'~'#0$#65535'
```
resolving the parsing ambiguity referred to in section 3.3.  It seems
more simpler to me that a parser would interpret the 1973 code to set
first element in array number 1 to the selection of every other bit
logical XOR of the third element of array number 1.  The document doesn't
say that array subscripts must be 16-bit, though it is somewhat implied,
but that might be one way to interpret the code as inverting the least
significant bit.  So, unless it turns out to be too difficult, this
implementation will only allow 16-bit array subscripts in order to follow
the 1973 document, which means the parser will be complicated by having
to distinguish between 16-bit and 32-bit expressions.

It is unclear whether `READING OUT` or simply `READING` is the gerund for
output, so this implementation will accept both.  Similarly for
`WRITING IN` versus `WRITING` for input.

Section 3.4.3 of the 1973 document says
```
#165¢'#203~#358'
```
is 34815, which was corrected to 34915 in the 1996 document.  In this
implementation, it evaluates to 34915.

Section 4.4.12 says that the elements in the `WRITE IN` list are separated
by intersections.  However, section 4.4.13 does not say that the elements
in the `READ OUT` list are separated by anything.  Since the list can
contain array elements, a separator is needed to prevent ambiguity
due to the possibility of multidimensional arrays, so this implementation
will require the list elements for `READ OUT` to be separated by
intersections.

Section 4.4.12 says that its arguments are "variables and/or elements or
arrays," which is probably a typo for "variables and/or elements *of*
arrays."  The typo remains in the 1996 document, but, by 1998, in the
C-INTERCAL 0.18 documentation, the "or" was changed to "of".  This
implementation extends `WRITE IN` to input binary when 16-bit arrays are in
the `WRITE IN` list as described later.  This method of binary input is
incompatible with the C-INTERCAL and the CLC-INTERCAL extensions for binary
input.

Section 4.4.13 omits arrays from the `READ OUT` list.  This implementation
extends `READ OUT` to output binary when 16-bit arrays are in the `READ OUT`
list as described later.  This method of binary output is incompatible with
the C-INTERCAL and the CLC-INTERCAL extensions for binary output.

Section 4.4.1 says 16-bit variables can only be assigned 32-bit values if
the value is less than 65535.  Presumably they can be assigned a 16-bit
value equal to 65535.  This bizarre restriction remains in the 1996
document.  This implementation will enforce it by giving a fatal error
if a 16-bit variable is assigned a 32-bit value greater than or equal to
65535.

This implementation treats a spark or a rabbit-ears that immediately
follows a binary operator or is immediately followed by a unary operator
as the left part of a pair, even if there is already a matching open
grouper to its left.  This is not explicitly stated in section 3.4.3 of
the document.  Otherwise, if there is a matching open grouper, it will be
treated as the right part of the pair.

Section 4.4.13 in the 1973 document says that `#3999` reads out as MMMIM,
which is what this implementation does.  The 1996 document says that
`#3999` reads out as MMMCMXCIX, which this implementation does not.

The document does not say what happens when variables are used before
being assigned.  It would make sense for a fatal error to occur in
most of the cases, though not in the cases of `IGNORE` or `REMEMBER`.
Since none of the error codes in the document seem to refer to using an
uninitialized variable, uninitialized variables will evaluate to `#0`,
and `STASHING` an uninitialized variable will stash `#0`.  Undimensioned
arrays will be 0-dimensional arrays, so evaluating any element will result
in error 241, and `STASHING` an undimensioned array will stash a
0-dimensional array.  After an array is dimensioned or redimensioned, all
the array elements contain `#0`.

The C-INTERCAL documentation says that spaces cannot be added inside a
keyword or a decimal number in an INTERCAL program.  Apparently C-INTERCAL
has no problems with spaces within numbers (labels, variables, constants).
In section 4.1 of the 1973 and 1996 document, the only restriction on spaces
is that no word of a statement identifier (`DO`, `PLEASE`, or `PLEASE DO`)
may contain any spaces.  In order to follow the original documents more
closely, this implementation allows spaces anywhere except within `DO` or
`PLEASE`, or within overpunches, i.e. `c<BACKSPACE><SPACE><BACKSPACE>/`
will not be recognized as change.

Since the errors in section 7 does not include the actual error messages,
and the messages for errors other than 000, 123, 275, and 436 are not
documented, the error messages are taken from the C-INTERCAL documentation.
Some research shows that the message for error 632 in C-INTERCAL differs
from the message in the 1972 implementation, so this implementation will use
the latter message.

Since section 7.1 does not say what the second line of an error message is
when there is no next statement, this implementation will have "ON THE
WAY TO STATEMENT nnnn" in those cases so that it still follows the format.

In section 4.4.7, the document says inputting into an `IGNOREd` variable
also has no effect.  This implementation interprets having no effect as
not receiving the input, so that it gets received by the next not `IGNOREd`
variable that is inputted into, as opposed to taking and discarding the
input and having to decide how to handle erroneous input.

Section 4.4.7 says all subsequent statements have no effect on the
variables and/or arrays.  That means `STASH` and `RETRIEVE` do nothing to
them.  Apparently, in C-INTERCAL, CLC-INTERCAL and J-INTERCAL, `STASH` and
`RETRIEVE` do have effects on `IGNOREd` variables, which seems incorrect to
me.

The INTERCAL System Library listed in section 6 of the document is not
included with this implementation.  It's probably a copyright violation to
include it.

This implementation includes an implementation of the INTERCAL System
Library as described in section 5 of the document.  `NEXTing` a label that
is not defined in the program and is the label of library function will
execute that function.  These labels cannot be `ABSTAINed FROM`, and the
functions are not affected by any `ABSTAINed FROM` gerunds.  The `IGNORE`
status of variables are respected by these library functions.

Extensions to INTERCAL
----------------------
When the `-strict` flag is passed the interpreter or compiler, statements
using these extensions will result in a fatal runtime error.

### Binary input
A 16-bit array argument to `WRITE IN` inputs bits.  If the array does not have
at least 1 element, error 241 results.  The first element contains the
number of consecutive zero bits initially input.  The second element contains
the number of consecutive one bits subsequently input.  The third element
contains the number of consecutive zero bits subsequently input, etc.
Consecutive elements with zero indicates that no more bits have been input,
otherwise subsequent bits can be input by subsequent `WRITE` statements.  An
implementation may split up runs of consecutive identical bits.  For example,
the last element of the array could be set to one, and a subsequent `WRITE`
would read the continuation of the run of bits.

This method of input can input a megabyte of data into a 129 element array,
for particular values of the data.  I don't think any major language has this
capability.

### Binary output
A 16-bit array argument of `READ OUT` statement will output the number of
bits equal to the name of the array.  For example, `,1` outputs one bit,
`,2` outputs two bits, `,10` outputs ten bits, etc.  The contents of the
array list the bit indexes of the one bits, with the other bits being zero.
Zeros are ignored.  Duplicate indexes and indexes greater than the number of
bits ought to result in some error, but are ignored.

For example, if `,65535` and `,8` are undimensioned,
```
    PLEASE READ OUT ,65535 + ,65535 + ,65535 + ,65535 + ,65535 + ,65535 + ,65535 + ,65535 + ,8
```
outputs 64 kilobytes of zeros.

### COME FROM and NEXT FROM
This implementation implements non-computed and gerund `COME FROM` and
`NEXT FROM`, which are described in the
[C-INTERCAL Manual](http://www.catb.org/~esr/intercal/ick.htm#COME-FROM-and-NEXT-FROM).

This implementation allows multiple `COME FROM` and `NEXT FROM` statements
to target a line.  It is only a run time error if the sum of the chance of
execution qualifiers of the non-abstained statements `COMING FROM` or
`NEXTING FROM` this line add up to more than 100%.  Otherwise, it randomly
goes to one of them (or none of them) based on their chance of execution
qualifiers.  Apparently, single-threaded C-INTERCAL version 0.30 disallows
this at compile time.

### Single bit output
This implementation extends INTERCAL to allow single bit output.
```
    PLEASE READ OUT
```
outputs a bit with value 1.

```
    PLEASE READ NAUGHT
```
or
```
    PLEASE READ NOT
```
or even
```
    PLEASE READN'T
```
outputs a bit with value 0.  I don't think any major language has even one
output bit statement, let alone two.

This implementation has a 7 bit buffer.  If the buffer is full, outputting
a bit causes the 8 bits to be written and the buffer to be cleared.  The
8 bits are written as a byte with the first bit buffered as the least
significant bit and the last bit as the most significant bit.  Any bits in
the buffer when the program terminates are discarded.

### Single bit input
This implementation extends INTERCAL to allow single bit input.
```
    PLEASE WRITE IN TO (label1+label2)
```
reads a bit.  If the bit is zero, jump to (label1).  If the bit is one,
jump to (label2).  If no bit can be read, continue to the next statement.
Neither label may be a library function.

This implementation has a 7 bit buffer.  If the buffer is empty, inputting
a bit causes 8 bits to be read as a byte.  The least significant bit is
returned, and the remaining bits are put in the buffer.  Subsequent inputs
removes the least significant remaining bit from the buffer and returns it
until the buffer is empty.

An optimization
---------------
If an array is `STASHed`, then immediately redimensioned, allocating and
copying into a new array when `STASHing` would be a waste, since it would
be freed when redimensioning.

The compiler in this implementation implements a lazy stashing optimization
for arrays that are `RETRIEVEd`.  These arrays have a stash count.  On
`STASH`, the stash count is incremented.  On `RETRIEVE`, the stash count
is checked.  If the stash count is greater than zero, it is decremented,
otherwise, the stash stack is popped.  On assigning an array element, the
stash count is checked.  If the stash count is greater than zero, it is
decremented and then the array value is pushed onto the stash stack and the
array value is set to a copy of it with the stash count set to zero.  Then,
in both cases, the stash count is zero and the array element is assigned.
On redimensioning the array, the stash count is checked.  If the stash
count is zero, the current array value is freed and a new array value is
allocated.  Otherwise, the stash count is decremented and then the array
value is pushed onto the stash stack and a new array value is allocated.

Name
----
PUKE is named for PUCC.PRINCETON.EDU, which, in the past, was an IBM 3081
and an IBM 3090.

Another idea is phoenick, for phoenix.princeton.edu, which, in the past,
was an Ultrix mainframe, a SunOS mainframe, and a Solaris mainframe.

Performance
-----------
Consider
[primes.i](https://gitlab.com/esr/intercal/blob/master/pit/primes.i).
Apparently in [1992](http://www.catb.org/~esr/intercal/stross.html),
it took 17 hours to run.  In 2018, compiled with ick-0.30, it runs in
14 seconds on a laptop.  Modified to add `(1001) DO RESUME .5` plus
a number of workarounds for the buggy PUKE implementations, it takes
27 seconds for COMPILAC to interpret, and when compiled by INTERLAC,
it runs in 7 seconds.
