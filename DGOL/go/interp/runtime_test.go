package main

import (
	"bufio"
	"bytes"
	"testing"
)

func TestVrbls(t *testing.T) {
	scope := newScope("")
	a := scope.getVrbl("A")
	if hasEdge(a, a) {
		t.Errorf("hasEdge(a,a)")
	}
	makeEdge(a, a)
	if !hasEdge(a, a) {
		t.Errorf("!hasEdge(a,a)")
	}
	removeEdge(a, a)
	if hasEdge(a, a) {
		t.Errorf("hasEdge(a,a)")
	}
	b := scope.getVrbl("B")
	if hasEdge(a, b) {
		t.Errorf("hasEdge(a,b)")
	}
	if hasEdge(b, a) {
		t.Errorf("hasEdge(b,a)")
	}
	if hasEdge(b, b) {
		t.Errorf("hasEdge(b,b)")
	}
	makeEdge(a, b)
	if !hasEdge(a, b) {
		t.Errorf("!hasEdge(a,b)")
	}
	alias := scope.getVrbl("A")
	if !hasEdge(alias, b) {
		t.Errorf("!hasEdge(alias,b)")
	}
	makeEdge(a, b)
	if !hasEdge(a, b) {
		t.Errorf("!hasEdge(a,b)")
	}
	scope.setVrbl("C", b)
	if !hasEdge(a, scope.getVrbl("C")) {
		t.Errorf("!hasEdge(a,C)")
	}
	removeEdge(a, b)
	if hasEdge(a, b) {
		t.Errorf("hasEdge(a,b)")
	}
	if hasEdge(alias, b) {
		t.Errorf("hasEdge(alias,b)")
	}
	removeEdge(a, b)
	if hasEdge(a, b) {
		t.Errorf("hasEdge(a,b)")
	}
	z := scope.getVrbl("0")
	if len(z.node.edges) != 0 {
		t.Errorf("len(z.node.edges)")
	}
}

func makeTestLibs(b *bytes.Buffer) map[string]map[string]func([]*vrbl) {
	testLibs := make(map[string]map[string]func([]*vrbl))
	testLib := make(map[string]func([]*vrbl))
	testLibs["TEST"] = testLib
	testLib["A"] = func([]*vrbl) {
		b.WriteRune('A')
	}
	testLib["B"] = func([]*vrbl) {
		b.WriteRune('B')
	}
	testLib["C"] = func([]*vrbl) {
		b.WriteRune('C')
	}
	testLib["D"] = func([]*vrbl) {
		b.WriteRune('D')
	}
	testLib["E"] = func([]*vrbl) {
		b.WriteRune('E')
	}
	testLib["F"] = func([]*vrbl) {
		b.WriteRune('F')
	}
	return testLibs
}

func testLinkError(t *testing.T, source []string, errMsg string) {
	var modules []*module
	for _, src := range source {
		module, err := parse(bufio.NewReader(bytes.NewBufferString(src)))
		if err != nil {
			t.Errorf("parse error: %s", err.Error())
			return
		} else {
			modules = append(modules, module)
		}
	}
	_, err := link(modules, nil)
	if err == nil {
		t.Errorf("link: err=nil")
	} else if err.Error() != errMsg {
		t.Errorf("link: err=%s", err.Error())
	}
}

func testRun(t *testing.T, source []string, expected string) {
	var modules []*module
	for _, src := range source {
		module, err := parse(bufio.NewReader(bytes.NewBufferString(src)))
		if err != nil {
			t.Errorf("parse error: %s", err.Error())
			return
		} else {
			modules = append(modules, module)
		}
	}
	testOutput := bytes.Buffer{}
	main, err := link(modules, makeTestLibs(&testOutput))
	if err != nil {
		t.Errorf("link: err=%s", err.Error())
		return
	}
	main()
	if testOutput.String() != expected {
		t.Errorf("run: output=%s", testOutput.String())
	}
}

func TestLinkRun(t *testing.T) {
	testLinkError(t, []string{
		`PROGRAM A
END A
`,
		`PROGRAM B
END B`,
	}, "MULTIPLE PROGRAM MODULES")
	testLinkError(t, []string{
		`SUBROUTINE A()
END A
LIBRARY A
  SUBROUTINE A
END A`,
		`SUBROUTINE B()
END B
LIBRARY A
  SUBROUTINE B
END A`,
	}, "DUPLICATE LIBRARY NAME: A")
	testLinkError(t, []string{
		`SUBROUTINE A()
END A
LIBRARY A
  SUBROUTINE A
END A`,
		`SUBROUTINE B()
END B
LIBRARY B
  SUBROUTINE B
END B`,
	}, "NO PROGRAM MODULE")
	testLinkError(t, []string{
		`SUBROUTINE A()
END A
LIBRARY A
  SUBROUTINE A
END A`,
		`SUBROUTINE B()
END B
LIBRARY B
  SUBROUTINE B
END B`,
		`USE A
USE B
PROGRAM C
  CALL A.B()
END C`,
	}, "UNRESOLVED CALL: A.B")
	testLinkError(t, []string{
		`SUBROUTINE A()
END A
LIBRARY A
  SUBROUTINE A
END A`,
		`USE A
SUBROUTINE B()
  CALL A.B()
END B
LIBRARY B
  SUBROUTINE B
END B`,
		`USE A
USE B
PROGRAM C
  CALL B.B()
END C`,
	}, "UNRESOLVED CALL: A.B")
	testLinkError(t, []string{
		`SUBROUTINE A()
  CALL A2()
END A
SUBROUTINE A2()
END A2
LIBRARY A
  SUBROUTINE A
END A`,
		`USE A
SUBROUTINE B()
  CALL A.A2()
END B
LIBRARY B
  SUBROUTINE B
END B`,
		`USE A
USE B
PROGRAM C
  CALL B.B()
END C`,
	}, "UNRESOLVED CALL: A.A2")

	testRun(t, []string{
		`USE TEST
PROGRAM A
  CALL TEST.A()
  CALL TEST.B()
  CALL TEST.C()
END A`,
	}, "ABC")

	testRun(t, []string{
		`USE TEST
SUBROUTINE A()
  CALL TEST.A()
  CALL TEST.B()
  CALL TEST.C()
END A
PROGRAM A
  CALL A()
END A`,
	}, "ABC")

	testRun(t, []string{
		`USE TEST
SUBROUTINE A(X)
  IF X > X
    CALL TEST.A()
  ELSE
    CALL TEST.B()
    RETURN
  END IF
  CALL TEST.C()
END A
PROGRAM A
  CALL A()
  LET Y = 0
  CALL A(Y)
  LET Y > Y
  CALL A(Y)
  LET Y < Y
  CALL A(Y)
END A`,
	}, "BBACB")

	testRun(t, []string{
		`USE TEST
SUBROUTINE A(X,Y)
  IF X = Y
    CALL TEST.A()
  ELSE
    IF X > Y
      CALL TEST.B()
    END IF
    IF Y > X
      CALL TEST.C()
    END IF
  END IF
  CALL TEST.D()
END A
PROGRAM A
  CALL A()
  LET I = J
  CALL A(I,J)
  LET J = 0
  CALL A(I,J)
  LET I > J
  CALL A(I,J)
  LET J > I
  CALL A(I,J)
  LET I < J
  CALL A(I,J)
END A`,
	}, "DADDBDBCDCD")

	testRun(t, []string{
		`USE TEST
PROGRAM A
  LET A > 0
  LET A > 0
  LET A > 0
  LET A > 0
  DO I < A
    CALL TEST.A()
  END DO
END A`,
	}, "AAAA")

	testRun(t, []string{
		`USE TEST
PROGRAM A
  LET A > 0
  LET A > 0
  LET A > 0
  LET A > 0
  DO I < A
    CALL TEST.A()
    EXIT I
  END DO
END A`,
	}, "A")

	testRun(t, []string{
		`USE TEST
SUBROUTINE A(X)
  DO I < X
    CALL TEST.A()
    RETURN
  END DO
END A
PROGRAM A
  LET A > 0
  LET A > 0
  LET A > 0
  LET A > 0
  CALL A(A)
END A`,
	}, "A")

	testRun(t, []string{
		`USE TEST
SUBROUTINE A(X)
  DO J
    DO I < X
      CALL TEST.A()
      EXIT J
    END DO
    RETURN
  END DO
  CALL TEST.B()
END A
SUBROUTINE B(X)
  DO J
    DO I < X
      CALL TEST.C()
      EXIT I
    END DO
    RETURN
  END DO
  CALL TEST.D()
END B
PROGRAM A
  LET A > 0
  LET A > 0
  LET A > 0
  LET A > 0
  CALL A(A)
  CALL B(A)
END A`,
	}, "ABC")

	testRun(t, []string{
		`USE TEST
SUBROUTINE Z()
  CALL TEST.A()
END Z
LIBRARY Z
  SUBROUTINE Z
END Z`,
		`USE Z
PROGRAM P
  CALL Z.Z()
END P`,
	}, "A")
}

func runExample(source []string) {
	var modules []*module
	for _, src := range source {
		module, err := parse(bufio.NewReader(bytes.NewBufferString(src)))
		if err != nil {
			println("parse error", err.Error())
			return
		} else {
			modules = append(modules, module)
		}
	}
	main, err := link(modules, makeStdlibs())
	if err != nil {
		println("link error", err.Error())
		return
	}
	main()
}

func ExampleUSEIO() {
	runExample([]string{
		`USE IO
PROGRAM USEIO
  LET B = 0
  LET EOF = 0
  LET 1 = 0
  LET 2 = 0
  LET 4 = 0
  LET 8 = 0
  LET 10 = 0
  LET 20 = 0
  LET 40 = 0
  LET 80 = 0
  CALL IO.READBYTE(B,EOF,1,2,4,8,10,20,40,80)
  IF B > EOF
    LET B > 1
    CALL IO.WRITEBYTE(B,0,0,0,1,0,0,1,0)
  ELSE
    LET B > 1
    CALL IO.WRITEBYTE(B,0,1,0,1,0,0,1,0)
  END IF
  CALL IO.WRITEBYTE(B,1,0,1,0,0,0,1,0)
  CALL IO.WRITEBYTE(B,0,0,1,1,0,0,1,0)
  CALL IO.WRITEBYTE(B,0,0,1,1,0,0,1,0)
  CALL IO.WRITEBYTE(B,1,1,1,1,0,0,1,0)
END USEIO
`,
	})
	// Output: HELLO
}
