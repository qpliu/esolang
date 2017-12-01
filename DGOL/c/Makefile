dgol: dgol.o dgol_libs.o indexer.o interpret.o mymalloc.o nodes.o parse.o resolver.o scope.o vars.o
	$(CC) -o $@ $^

clean:
	$(RM) *.o dgol

.PHONY: clean deps

deps:
	for f in *.c; do echo $$(basename $$f .c).o: $$(cc -E $$f | sed -ne 's/^# 1 "[.].\([^"]*\)".*$$/\1/p'); done

dgol.o: ast.h dgol_libs.h scope.h vars.h interpret.h resolver.h parse.h
dgol_libs.o: dgol_libs.h scope.h vars.h
indexer.o: indexer.h mymalloc.h
interpret.o: ast.h interpret.h resolver.h dgol_libs.h scope.h vars.h
mymalloc.o: mymalloc.h
nodes.o: mymalloc.h nodes.h
parse.o: ast.h mymalloc.h parse.h
resolver.o: dgol_libs.h scope.h vars.h indexer.h mymalloc.h resolver.h ast.h
scope.o: mymalloc.h scope.h vars.h
vars.o: mymalloc.h nodes.h vars.h
