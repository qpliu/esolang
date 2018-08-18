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

Notes
-----
Since the famous politeness checker was undocumented, it will not be
part of this implemention.

In section 4.4.1 of the 1973 document says
  to invert the least significant bit of the first element of 16-bit
  2-dimensional array number 1, one could write:
```
    ,1SUB#1#1 <- ’⊻,1SUB#1#1¢#1’~’#0¢#65535’
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
examples on the internet of READING OUT arrays.

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
