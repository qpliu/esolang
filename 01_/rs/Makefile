SOURCES=ast.rs bits.rs bits1.rs error.rs interp.rs interp1.rs interp2.rs interp3.rs lazylist.rs location.rs parse1.rs ri01_.rs symbol.rs token.rs

ri01_: $(SOURCES)
	rustc ri01_.rs

test01_: $(SOURCES)
	rustc --test -o test01_ ri01_.rs

test: test01_
	./test01_

clean:
	$(RM) ri01_ test01_
