SOURCES=ast.rs bits.rs error.rs interp.rs lazylist.rs location.rs parse1.rs ri01_.rs symbol.rs token.rs

r01_: $(SOURCES)
	rustc ri01_.rs

test01_: $(SOURCES)
	rustc --test -o test01_ ri01_.rs

test: test01_
	./test01_

clean:
	$(RM) ri01_ test01_
