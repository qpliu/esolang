This is an implementation of the INTERCAL programming language
described by  http://www.muppetlabs.com/~breadbox/intercal/intercal.txt
with the exception of using Unicode instead of EBCDIC.

COMPILAC is an interpreter.

INTERLAC is a compiler targeting LLVM version 5.  It probably needs
only minimal changes for later LLVM versions.  For example,
`@llvm.memset.p0i8.i32` and `@llvm.memcpy.p0i8.p0i8.i32` drop their align
arguments at some later version (somewhere from LLVM version 6 to LLVM
version 8).

This implementation attempts to follow the 1973 document in
preference to any later changes or additions.  As the 1973 document
references the Atari Implementation, it probably includes revisions
from the 1980s.  It seems reasonable to me to assume that these
revisions are limited to the Notes On The Atari Implementation and
the ASCII entries in the character set table.

Please note that this implementation is very buggy.

Known bugs
----------
This implementation does not allow spaces within `NOT`, `N'T`, `NEXT`,
`FORGET`, `CALCULATING`, `UP`, `BY`, `SUB`, etc.  According to 4.1, only
`DO` and `PLEASE` cannot contain spaces.

Notes
-----
Since the famous politeness checker was undocumented, it will not be
part of this implemention.

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

Section 3.4.2 of the 1973 document says
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

Section 4.4.13 also omits arrays from the `READ OUT` list, so this
implementation will not support arrays in the `READ OUT` list despite many
examples on the internet of `READING OUT` arrays.  Apparently, that is
for binary or text I/O in newer implementations.

Section 4.4.1 says 16-bit variables can only get 32-bit values if the
value is less than 65535.  Presumably they can get a 16-bit value equal
to 65535.  This bizarre restriction remains in the 1996 document.  This
implementation will enforce it.

This implementation treats a spark or a rabbit-ears that immediately
follows a binary operator or is immediately followed by a unary operator
as the left part of a pair, even if there is already a matching open
grouper to its left.  This is not explicitly stated in section 3.4.3 of
the document.  Otherwise, if there is a matching open grouper, it will be
treated as the right part of the pair.

Section 4.4.12 does not specify how arrays are written in.  For
arrays without initial dimensioning, this implementation will result in
a fatal error.  For one-dimensional arrays, the only obvious
interpretation is to read starting with subscript 1 and ending with the
largest subscript.  For multi-dimensional arrays, the leftmost subscript
will will run through its entire range from 1 to its largest subscript,
then reset and run through its entire range again with the next leftmost
subscript incremented to its next value, and similarly for all its
subscripts until all the subscripts have their largest values.

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
decimal number in an INTERCAL program.  This restriction is not in the
1973 document or the 1996 document, so this implementation allows spaces
(and line breaks) inside numbers.

Since section 4.4.11 says that `PLEASE GIVE UP` has the effect of a `PLEASE
RESUME #80`, this implementation will never produce error 632.  On second
thought, this implementation will produce error 632 when terminating via
a resume statement, and `PLEASE GIVE UP` will be different than `PLEASE
RESUME #80`, as the former will terminate without an error and the latter
will terminate with error 632.

Since the errors in section 7 does not include the actual error messages,
and the messages for errors other than 000, 123, 275, and 436 are not
documented, the error messages are taken from the C-INTERCAL documentation,
with the exception of error 579 when `WRITING IN` an empty line, which is
"WHAT YOU WRITE DOES NOT COUNT" in this implementation.  On second
thought, an empty line shall result in error 562, instead of inventing
messages that are not in any of the documents.  Some research shows that
the message for error 632 in C-INTERCAL differs from the message in the
1972 implementation, so this implementation will use the latter message.

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
them.  Apparently, in J-INTERCAL and CLC-INTERCAL, `STASH` and `RETRIEVE` do
have effects on `IGNOREd` variables, which seems incorrect to me.

The INTERCAL System Library described in sections 5 and 6 is not
included with this implementation.  It's probably a copyright violation
to include it.  An optimized INTERCAL System Library implementation
would behave differently from an INTERCAL implementation when `ABSTAIN` or
`IGNORE` are used or when operating close to the `RESUME` stack limit.
Perhaps a future version of this implementation will include such an
optimized INTERCAL System Library.

Extensions to INTERCAL
----------------------
This implementation extends INTERCAL to allow binary output.

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
outputs a bit with value 0.

This implementation has a 7 bit buffer.  If the buffer is full, outputting
a bit causes the 8 bits to be written and the buffer to be cleared.  The
8 bits are written as a byte with the first bit buffered as the least
significant bit and the last bit as the most significant bit.  Any bits in
the buffer then the program terminates are discarded.
