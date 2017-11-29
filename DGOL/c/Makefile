dgol: dgol.o dgol_libs.o indexer.o mymalloc.o nodes.o resolver.o scope.o vars.o
	$(CC) -o $@ $^

clean:
	$(RM) *.o dgol

.PHONY: clean deps

deps:
	for f in *.c; do echo $$(basename $$f .c).o: $$(cc -E $$f | sed -ne 's/^# 1 "[.].\([^"]*\)".*$$/\1/p'); done

dgol.o:
dgol_libs.o: dgol_libs.h scope.h vars.h
indexer.o: indexer.h mymalloc.h
mymalloc.o: mymalloc.h
nodes.o: mymalloc.h nodes.h
resolver.o: dgol_libs.h scope.h vars.h indexer.h mymalloc.h resolver.h ast.h
scope.o: mymalloc.h scope.h vars.h
vars.o: mymalloc.h nodes.h vars.h
