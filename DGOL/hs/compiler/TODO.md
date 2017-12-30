TODO List

* ~~Add DEBUG TRACE to DGOLLibs, subroutines include~~
  - ~~LABEL~~
  - ~~NODE~~
  - ~~FRAME~~

* ~~Debug compiler output for DO EDGES~~

* ~~Rework parser to include source locations~~

* ~~Debug compiler output for BRAINFUCK.DGOL~~
  - ~~Fix infinite loop in garbage collector~~
  - ~~Fix infinite loop due to EXIT LOOP at line 392 branching to %2004
    instead of %2010~~ (workaround)

* Track down why some br instructions aren't emitted, causing the
  weird infinite loops in BRAINFUCK.DGOL

* Write up on implementation
  - Call frame data structure
    + Notes on call by reference
    + Notes on DO EDGES
  - Allocation page data structure
    + Notes on garbage collection strategy

* Write up on compacting garbage collector

* Add source code metadata
  - DIFile(filename)
  - DISubprogram(name,file,line,isLocal,isDefinition)
  - DILocation(line,scope)
  - DILocalVariable(name,arg)

* Add debugging metadata
  - call llvm.dbg.addr (in prelude for each local variable)
  - call llvm.dbg.value (when doing LET assign)

* Implement compacting garbage collector
