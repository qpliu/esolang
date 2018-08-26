COMPILAC is an attempt to implement an interpreter of the programming
language as described by
http://www.muppetlabs.com/~breadbox/intercal/intercal.txt
with the exception of using Unicode instead of EBCDIC.

Also, the implementation attempts to follow the 1973 document in
preference to any later changes or additions.  As the 1973 document
references the Atari Implementation, it probably includes revisions
from the 1980s.  It seems reasonable to me to assume that these
revisions are limited to the Notes On The Atari Implementation and
the ASCII entries in the character set table.

I will try to avoid looking at other implementations until the
implementation is reasonably complete so that it will be based on
the documents rather than how another implementation did it.

Known Bugs
----------
The expression parser fails to parse some (many or most) complicated
expressions.

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

It is unclear whether READING OUT or simply READING is the gerund for
output, so this implementation will accept both.  Similarly for
WRITING IN versus WRITING for input.

Section 3.4.2 of the 1973 document says
```
#165¢'#203~#358'
```
is 34815, which was corrected to 34915 in the 1996 document.  In this
implementation, it evaluates to 34915.

Section 4.4.12 says that the elements in the WRITE IN list are separated
by intersections.  However, section 4.4.13 does not say that the elements
in the READ OUT list are separated by anything.  Since the list can
contain array elements, a separator is needed to prevent ambiguity
due to the possibility of multidimensional arrays, so this implementation
will require the list elements for READ OUT to be separated by
intersections.

Section 4.4.13 also omits arrays from the READ OUT list, so this
implementation will not support arrays in the READ OUT list despite many
examples on the internet of READING OUT arrays.  Apparently, that is
for binary or text I/O in newer implementations.

Section 4.4.1 says 16-bit variables can only get 32-bit values if the
value is less than 65535.  Presumably they can get a 16-bit value equal
to 65535.  This bizarre restriction remains in the 1996 document, so
this implementation will enforce it.

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
then reset to 1 with the next leftmost subscript incremented to its
next value, and similarly for all its subscripts until all the subscripts
have are at their largest values.

Section 4.4.13 in the 1973 document says that #3999 reads out as MMMIM,
which is what this implementation does.  The 1996 document says that
#3999 reads out as MMMCMXCIX, which this implementation does not.

The document does not say what happens when variables are used before
being assigned.  It would make sense for a fatal error to occur in
most of the cases, though not in the cases of IGNORE or REMEMBER.  Since
none of the error messages in the document seem to refer to using an
uninitialized variable, uninitialized variables will evaluate to 0,
and STASHING an uninitialized variable will stash 0.  For undimensioned
arrays, it will be a 0-dimensional array, so evaluating any element
will result in error 241, and STASHING an undimensioned array will stash
a 0-dimensional array.  After an array is dimensioned or redimensioned,
all array elements contain 0.

The C-INTERCAL documentation says that spaces cannot be added inside a
decimal number in an INTERCAL program.  This restriction is not in the
1973 document or the 1996 document, so this implementation allows spaces
inside numbers.

Since section 4.4.11 says that PLEASE GIVE UP has the effect of a PLEASE
RESUME #80, this implementation will never produce error 632.  On second
thought, this implementation will produce error 632 when terminating via
a resume statement, and PLEASE GIVE UP will be different than PLEASE
RESUME #80, as the former will terminate without an error and the latter
will terminate with error 632.

Since the errors in section 7 does not include the actual error messages,
and the messages for errors other than 000, 123, 275, and 436 are not
documented, the error messages are taken from the C-INTERCAL documentation,
with the exception of error 579 when WRITING IN an empty line, which is
"WHAT YOU WRITE DOES NOT COUNT" in this implementation.  On second
thought, an empty line shall result in error 562, instead of inventing
messages that are not in any of the documents.

Since section 7.1 does not say what the second line of an error message is
when there is no next statement, this implementation will have "ON THE
WAY TO STATEMENT nnnn" in those cases so that it still follows the format.

In section 4.4.7, the document says inputting into an IGNOREd variable
also has no effect.  This implementation interprets having no effect as
not receiving the input, so that it gets received by the next not IGNOREd
variable that is inputted into, as opposed to taking and discarding the
input and having to decide how to handle erroneous input.

The INTERCAL System Library described in sections 5 and 6 is not
included with this implementation.  It's probably a copyright violation
to include it.  An optimized INTERCAL System Library implementation
would behave differently from an INTERCAL implementation when ABSTAIN or
IGNORE are used or when operating close to the RESUME stack limit.
Perhaps a future version of this implementation will include such an
optimized INTERCAL System Library.

Compiler
--------
INTERLAC is an attempt to implement a compiler.

## Notes on compiler implementation
Use the COMPILAC parser.

Generate LLVM assembly.  Don't want to deal with LLVM bindings.

All code generation goes into @main.  There may be other runtime functions.

Global variables include an ABSTAIN array [n x i1], a NEXT stack [79 x i8*],
a NEXT stack pointer i8, one for each variable and array referenced in
the program, a buffer [5 x i8] for WRITE IN.  These don't have to be globals.
They could be allocaed in @main (or, for WRITE IN, in the runtime function).

Each statement will have an entry LLVM label that will be used by the NEXT
stack and other statements.  All other LLVM labels in the statement will
be private to the statement.  The statement code has the abstain check and
the chance check if necessary.

Use blockaddress for NEXT and indirectbr for RESUME.

Variables will have type { i1, variable_data* }, where the flag is the IGNORE
flag.  variable_data is a linked list for STASH/RETRIEVE and has type
{ variable_data*, u32 }.

Arrays will have type { i1, array_data* }, where the flag is the IGNORE
flag.  array_data is a linked list for STASH/RETRIEVE and has type
{ array_data*, u32, [n x u32], [0 x u32] }, where the second element is the
number of dimensions, the third element is the dimensions (n is the maximum
number of dimensions determined at compile time), and the fourth element is
the array values.  It may be possible to have an array_data typedef for
each array variable, each with its own maximum number of dimensions.

Run-time values will have type { i2, u32 }, where the first value is the
type tag where 0 is for 16-bit values, 1 is for 32 bit values, and 2
is for most error codes, and 3 is for error code 579 that requires
special processing.

For error code 579, to avoid buffering a potentially unlimited number of
characters for the error message, the bytes (after the first 5, which
are buffered) will be copied byte by byte from the input file to the output
file.

An alternative to using a [5 x i8] buffer for WRITE IN could be having a
big mess of branches on reading each character.  It wouldn't be that
horrible, being hand-written LLVM assembly for the runtime library.

Have a [n x i8] constant for the program listing, written at the start of
execution.  Calculate offsets into it for error code 000 messages.  Also
calculate offsets into it for line numbers for the ON THE WAY TO STATEMENT
nnnn part of error messages.
