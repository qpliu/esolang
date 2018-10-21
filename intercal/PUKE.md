PUKE is a new implementation of INTERCAL.  PUKE implements INTERCAL-72
using Unicode instead of EBCDIC plus extensions for single-threaded
COME FROM/NEXT FROM and binary input and output.

PUKE includes an interpreter and a compiler.  The compiler compiles
INTERCAL to the LLVM assembly language.

It runs on macOS 10.13 with Go 1.9 and LLVM versions 3, 5, 6, and 7.
It probably works with other versions and on other platforms, but
that's never been tried.

The source code is at
https://github.com/qpliu/esolang/tree/master/intercal

Getting started
---------------

    $ make

builds COMPILAC, the interpreter, and INTERLAC, the compiler.

    $ ./COMPILAC ./example/hello.i

runs an example program.

    $ ./INTERLAC ./example/hello.i

compiles the example program to ./a.out.

    $ ./a.out

Runs the compiled example program.

To run a bigger example,

    $ ./COMPILAC -q ./example/ADVENT.I

It may take some time to parse the more than 71 thousand statements.
(It compiles to 4.78 million lines of LLVM, with just about all of it
in a single function, so don't try compiling it.)

Some differences from C-INTERCAL
--------------------------------
Section 3.4.1 of the original INTERCAL reference manual says, "If more
than 16 bits are selected, the result is a 32-bit value, otherwise it
is a 16-bit value."

    PLEASE DO .1 <- 'V#1~"#0¢#1"'

C-INTERCAL version 0.30 results in error 275.  PUKE assigns #32769 to
.1.

Section 4.1 of the original INTERCAL reference manual says, "Spaces
may be used freely to enhance program legibility (or at least reduce
program illegibility), with the restriction that no word of a
statement identifier (see section 4.3) may contain any spaces."

    PLEASE NO TAB STAIN FROM (1)

C-INTERCAL version 0.30 results in error 000.  PUKE recognizes the
abstained ABSTAIN statement.

Duplicate labels in C-INTERCAL result in a compile time error.
Duplicate labels in PUKE result in a run time error if a statement
with a duplicate label is executed or if a statement referencing a
duplicate label is executed.

Section 4.4.1 of the original INTERCAL reference manual says, "16-bit
variables may be assigned 32-bit values only if the value is less than
65535."

    DO :1 <- #65535
    PLEASE .1 <- :1

C-INTERCAL version 0.30 assigns #65535 to .1.  PUKE results in error
275.

Section 4.4.7 of the original INTERCAL reference manual says, "The
statement DO IGNORE list causes all subsequent statements to have no
effect upon variables and/or arrays named in the list."

    DO .1 <- #1
    DO STASH .1
    DO .1 <- #2
    DO STASH .1
    PLEASE IGNORE .1
    PLEASE RETRIEVE .1
    DO REMEMBER .1
    DO RETRIEVE .1
    DO READ OUT .1 

C-INTERCAL outputs I because the RETRIEVE under the IGNORE pops
the stash stack of .1.  PUKE outputs II because the RETRIEVE under the
IGNORE has no effect on .1.

Section 4.4.7 of the original INTERCAL reference manual says,
"Inputting (see section 4.4.12) into an IGNOREd variable also has no
effect."

    DO IGNORE .1
    PLEASE WRITE IN .1

C-INTERCAL version 0.30 has the side-effect of inputting a number.
PUKE does not input a number.

Section 4.4.13 of the original INTERCAL reference manual says, "#3999
would read out as MMMIM".

    PLEASE READ OUT #3999

C-INTERCAL version 0.30 outputs MMMCMXCIX.  PUKE outputs MMMIM.

Section 7.10 of the C-INTERCAL 0.29 manual says, "[...] if two COME
FROMs or NEXT FROMs aim at the same line.  In a non-multithreaded
program [...], this is an error; but it is only an error if the
statement that they both point to finishes running, and both COME
FROMs or NEXT FROMs try to execute as a result (they might not if, for
instance, one is abstained or has a double-oh-seven causing it not to
run some of the time)."  Single-threaded C-INTERCAL version 0.30
disallows multiple COME FROMs/NEXT FROMs referencing the same
statement at compile time.  I assume that's a bug in C-INTERCAL
version 0.30.  PUKE only gives an error at run time if sum of the
chances of execution of the non-abstained COME FROMs/NEXT FROMs is
greater than 100%.

PUKE's extensions for binary input and output, though syntactically
identical to, are not compatible with those of C-INTERCAL.  With PUKE,
READing OUT a 16-bit array outputs binary data.  The number of bits
output is given by the name of the array.  The contents of the array
are the indices of the 1 bits.  Elements containing #0 are ignored.
Duplicate elements other than #0 or elements greater than the number
of bits ought to result in an error, but are ignored.  WRITing IN to a
16-bit array inputs binary data.  If the array is not dimensioned to
have at least 1 element, error 241 results.  The resulting first
element of the array is the number of consecutive zeros at the
beginning of the input.  The second element is the number of
consecutive ones that follow.  The third element is the number of
consecutive zeros that follow the ones, and so on.  Consecutive zeros
in the array indicate the end of input.  Runs of consecutive bits may
be broken up into multiple runs of 1 or more (to avoid requiring
input buffering). Indices to the left vary more rapidly than indices to
the right when determining the order of elements in multi-dimensional
arrays.
