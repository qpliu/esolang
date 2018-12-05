The 316 virtual machine
=======================
The 316 processor has a 1-bit register (R), and a 16-bit instruction
opcode pointer (P3) and a 16-bit instruction operand pointer (P16),
and can address 65536 bits of memory.

On initialization, R, P3, and P16 all contain 0.

On execution, the opcode is loaded from MEM[P3], MEM[P3+1], and
MEM[P3+2], and the operand is loaded from MEM[P16-1], MEM[P16-2],
... MEM[P16-16].  When the operand is an address, MEM[P16-1] is the
least significant bit and MEM[P16-16] is the most significant bit.  P3
then becomes P3+3 and P16 becomes P16-16.

Instruction set
---------------
Opcode | Instruction | Description
------ | ----------- | -----------
000    | NOP         | No operation
001    | LDR         | Load R from operand address
010    | STR         | Store R to operand address
011    | JZ3         | P3 jumps to operand address if R is zero
100    | JZ16        | P16 jumps to operand address if R is zero
101    | ANDR        | Set R to R AND MEM[P16-1]
110    | ORR         | Set R to R OR MEM[P16-1]
111    | XORR        | Set R to R XOR MEM[P16-1]

Input
-----
Storing to MEM[0x8000] will poll for input.  Storing 0 to MEM[0x8000]
will poll with a timeout.  Storing 1 to MEM[0x8000] will poll and
block until an input event is received.

MEM[0x8000] will contain 0 if no input event was received during the
last poll for input, and will contain 1 if an input event was
received.

MEM[0x8001] .. MEM[0x8004] contains the input event code.

Output
------
MEM[0x6000] .. MEM[0x77ff] is a 128x48 frame buffer.

316 assembler
=============

Example
-------
Hello world
```
@z:   JZ16 @o.16
@o:   ORR 1
      STR 8000
      ANDR 0
      JZ3 @z.3
      NOP @o.16
6000: 01010111010001000111000101010111011001000110010
6080: 01010100010001000101000101010101010101000101010
6100: 01110111010001000101000101010101011001000101010
6180: 01010100010001000101000101010101010101000101000
6200: 01010111011101110111000010100111010101110110010
```
Assembles to
```
0000: 001 011 010 101 110 000 ...
6000: 01010111010001000111000101010111011001000110010 ...
6080: 01010100010001000101000101010101010101000101010 ...
6100: 01110111010001000101000101010101011001000101010 ...
6180: 01010100010001000101000101010101010101000101000 ...
6200: 01010111011101110111000010100111010101110110010 ...
FFA0: 1111 1111 1111 0000
FFB0: 0000 0000 0000 0000
FFC0: 0000 0000 0000 0000
FFD0: 1000 0000 0000 0000
FFE0: 0000 0000 0000 0001
FFF0: 1111 1111 1111 0000
```
