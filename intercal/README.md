ILWNPA is an attempt to implement an interpreter of the programming
language as described by
http://www.muppetlabs.com/~breadbox/intercal/intercal.txt
with the exception of using Unicode instead of EBCDIC.

Also, the implementation attempts to follow the 1973 document in
preference to any later changes or additions.  As the 1973 document
references the Atari Implementation, it probably includes revisions
from the 1980s.  It seems reasonable to me to assume that these
revisions are limited to the Notes On The Atari Implementation and
the ASCII entries in the character set table.

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
first element in array number 1 to the selection of the bits in
1431655765 indicated by the bits in logical XOR of the third element
of array number 1.  The document doesn't say that array subscripts
must be 16-bit, though it is somewhat implied, but that might be one
way to interpret the code as inverting the least significant bit.  So,
unless it turns out to be too difficult, this implementation will only
allow 16-bit array subscripts in order to follow the 1973 document,
which means the parser will be complicated by having to distinguish
between 16-bit and 32-bit expressions.

It is unclear whether READING OUT or simply READING is the gerund for
output, so this implementation will accept both.  Similarly for
WRITING IN versus WRITING for input.
