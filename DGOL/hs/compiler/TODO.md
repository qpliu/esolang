TODO List

* Debug compiler output for DO EDGES

* Rework parser to include source locations

* Add source code metadata
  - DIFile(filename)
  - DISubprogram(name,file,line,isLocal,isDefinition)
  - DILocation(line,scope)
  - DILocalVariable(name,arg)

* Add debugging metadata
  - call llvm.dbg.addr (in prelude for each local variable)
  - call llvm.dbg.value (when doing LET assign)

* Write up on implementation
  - Call frame data structure
    + Notes on call by reference
    + Notes on DO EDGES
  - Allocation page data structure
    + Notes on garbage collection strategy

* Write up on compacting garbage collector

* Implement compacting garbage collector
