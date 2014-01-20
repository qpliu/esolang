SOURCES=ast.rs bits.rs error.rs interp.rs lazylist.rs location.rs parse1.rs r01_.rs symbol.rs token.rs

r01_: $(SOURCES)
	rustc r01_.rs

test01_: $(SOURCES)
	rustc --test -o test01_ r01_.rs

test: test01_
	./test01_

clean:
	$(RM) r01_ test01_
