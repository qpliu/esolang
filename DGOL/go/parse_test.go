package main

import (
	"bufio"
	"bytes"
	"io"
	"testing"
)

const example = `USE LIBRARY
SUBROUTINE HOME(X, Y)
  IF X = Y
    RETURN
  ELSE IF X > Y
    DO I < X
      LET X < I
    END DO
  ELSE
    CALL LIBRARY.DIE()
  END IF
  LET X > 0
END HOME
PROGRAM TEST
  LET A = 0
  LET B = 0
  LET A > B
  DO C
    LET B > C
    CALL HOME(A, C)
    EXIT C
  END DO
END TEST
`

func TestParse(t *testing.T) {
	testParseSuccess(t, `PROGRAM TEST
END TEST
`)
	testParseError(t, `PROGRAM TEST
END TEST2
`, "SYNTAX ERROR: ENDTEST2")

	testParseSuccess(t, `SUBROUTINE TEST()
END TEST
LIBRARY TEST
  SUBROUTINE TEST
END TEST
`)
	testParseError(t, "", "EOF")
	testParseError(t, `USE A
USE A
`, "DUPLICATE USE: USEA")
	testParseError(t, `USE 0
`, "SYNTAX ERROR: USE0")
	testParseError(t, `CALL ME()
`, "SYNTAX ERROR: CALLME()")
	testParseError(t, `SUBROUTINE
END SUBROUTINE
`, "SYNTAX ERROR: SUBROUTINE")
	testParseError(t, `SUBROUTINE A()
END A
SUBROUTINE A(X)
END A
`, "DUPLICATE SUBROUTINE: SUBROUTINEA(X)")
	testParseError(t, `PROGRAM 0
END 0
`, "SYNTAX ERROR: PROGRAM0")
	testParseError(t, `PROGRAM X(Y,Z)
END X
`, "SYNTAX ERROR: PROGRAMX(Y,Z)")
	testParseError(t, `SUBROUTINE X(Y,Z)
END X
LIBRARY $
END $
`, "SYNTAX ERROR: LIBRARY$")
	testParseError(t, `SUBROUTINE X(Y,Z)
END X
LIBRARY X
  SUBROUTINE Y
END X
`, "UNDEFINED SUBROUTINE: Y")
	testParseError(t, `SUBROUTINE X(Y,Z)
END X
CALL X(0,0)
`, "SYNTAX ERROR: CALLX(0,0)")
	testParseError(t, `SUBROUTINE X(Y,Z)
  CALL X
END X
`, "SYNTAX ERROR: CALLX")
	testParseError(t, `SUBROUTINE X(Y,Z)
END X
`, "EOF")
	testParseError(t, `PROGRAM X
END X
MORE JUNK
`, "SYNTAX ERROR: MOREJUNK")
	testParseSuccess(t, `USE X
SUBROUTINE Y()
  CALL X.Y()
END Y
PROGRAM X
  CALL Y()
  CALL X.Y()
END X
`)
	testParseError(t, `USE X
SUBROUTINE Y()
  CALL X.Y()
  CALL Z()
END Y
PROGRAM X
  CALL Y()
  CALL X.Y()
END X
`, "UNDEFINED SUBROUTINE: Z")
	testParseError(t, `USE X
SUBROUTINE Y()
  CALL X.Y()
END Y
PROGRAM X
  CALL Y()
  CALL X.Y()
  CALL Z()
END X
`, "UNDEFINED SUBROUTINE: Z")
	testParseError(t, `USE X * TEST COMMENT
* TEST COMMENT
USE X * DUPLICATE`, "DUPLICATE USE: USEX")
	testParseError(t, `SUBROUTINE Y(0)
END Y
PROGRAM X
  CALL Y()
END X
`, "SYNTAX ERROR: SUBROUTINEY(0)")
	testParseError(t, `SUBROUTINE Y(A)
`, "EOF")
	testParseSuccess(t, `SUBROUTINE Y(A, B)
  LET C = A
  LET D > 0
  LET A < B
END Y
PROGRAM Z
END Z
`)
	testParseError(t, `SUBROUTINE Y(A, B)
  LET C
END Y
PROGRAM Z
END Z
`, "SYNTAX ERROR: LETC")
	testParseError(t, `SUBROUTINE Y(A, B)
  LET C < 0
END Y
PROGRAM Z
END Z
`, "SYNTAX ERROR: LETC<0")
	testParseError(t, `SUBROUTINE Y(A, B)
  LET C : 0
END Y
PROGRAM Z
END Z
`, "SYNTAX ERROR: LETC:0")
	testParseSuccess(t, `SUBROUTINE Y(A, B)
  IF A = B
    RETURN
  ELSE IF A > B
  ELSE
    IF A > A
    END IF
  END IF
END Y
PROGRAM Z
END Z
`)
	testParseError(t, `PROGRAM X
  IF A = B
    IF B
    END IF
  END IF
END X
`, "SYNTAX ERROR: IFB")
	testParseError(t, `PROGRAM X
  IF A = B
    IF B = C
      LET D > 0
    END IF B
  END IF
END X
`, "SYNTAX ERROR: ENDIFB")
	testParseError(t, `PROGRAM X
  IF A = B
  ELSE IF B = 0
  END IF
END X
`, "SYNTAX ERROR: ELSEIFB=0")
	testParseError(t, `PROGRAM X
  IF A = B
  ELSE END
  END IF
END X
`, "SYNTAX ERROR: ELSEEND")
	testParseError(t, `PROGRAM X
  IF A = B
  ELSE
    LET A = B
  ELSE IF B > A
  END IF
END X
`, "SYNTAX ERROR: ELSEIFB>A")
	testParseSuccess(t, `PROGRAM TESTDO
  DO I
  END DO
END TESTDO
`)
	testParseSuccess(t, `PROGRAM TESTDO
  DO I < NODE
  END DO
END TESTDO
`)
	testParseError(t, `PROGRAM TESTDO
  DO I = NODE
  END DO
END TESTDO
`, "SYNTAX ERROR: DOI=NODE")
	testParseError(t, `PROGRAM TESTDO
  DO #
  END DO
END TESTDO
`, "SYNTAX ERROR: DO#")
	testParseError(t, `PROGRAM TESTDO
  DO I
    ERROR
  END DO
END TESTDO
`, "SYNTAX ERROR: ERROR")
	testParseError(t, `PROGRAM TESTCALL
  CALL Z(1,,2)
END TESTCALL
`, "SYNTAX ERROR: CALLZ(1,,2)")
	testParseSuccess(t, `SUBROUTINE S(1, 2)
  CALL S(2, 1)
END S
PROGRAM TESTCALL
END TESTCALL
`)
	testParseError(t, `PROGRAM TESTRETURN
  RETURN
END TESTRETURN
`, "SYNTAX ERROR: RETURN")
	testParseError(t, `SUBROUTINE RETURNA()
  RETURN A
END RETURNA
PROGRAM TESTRETURN
END TESTRETURN
`, "SYNTAX ERROR: RETURNA")
	testParseSuccess(t, `PROGRAM TESTEXIT
  DO I
    EXIT I
  END DO
END TESTEXIT
`)
	testParseSuccess(t, `PROGRAM TESTEXIT
  DO I
    DO J
      EXIT I
    END DO
  END DO
END TESTEXIT
`)
	testParseSuccess(t, `PROGRAM TESTEXIT
  DO I
    DO J
      EXIT J
    END DO
  END DO
END TESTEXIT
`)
	testParseError(t, `PROGRAM TESTEXIT
  DO I
    DO J
      EXIT K
    END DO
  END DO
END TESTEXIT
`, "SYNTAX ERROR: EXITK")
	testParseError(t, `PROGRAM TESTEXIT
  DO I
    DO J
    END DO
    EXIT J
  END DO
END TESTEXIT
`, "SYNTAX ERROR: EXITJ")
	testParseError(t, `PROGRAM TESTEXIT
  DO I
    DO J
      EXIT 0
    END DO
  END DO
END TESTEXIT
`, "SYNTAX ERROR: EXIT0")
	testParseError(t, `SUBROUTINE A()
  CALL A[]
END A`, "SYNTAX ERROR: CALLA[]")
	testParseError(t, `SUBROUTINE A(A,A)
END A`, "SYNTAX ERROR: SUBROUTINEA(A,A)")
	testParseError(t, `SUBROUTINE A(A.A)
END A`, "SYNTAX ERROR: SUBROUTINEA(A.A)")
	testParseError(t, `LIBRARY A
  SUBROUTINE
END A
`, "SYNTAX ERROR: SUBROUTINE")
	testParseError(t, `LIBRARY A
  SUBROUTINE 0
END A
`, "SYNTAX ERROR: SUBROUTINE0")
	testParseError(t, `LIBRARY A
  SUBROUTINE B()
END A
`, "SYNTAX ERROR: SUBROUTINEB()")
	testParseError(t, `SUBROUTINE B()
END B
LIBRARY A
  SUBROUTINE B
END C
`, "SYNTAX ERROR: ENDC")
	testParseError(t, `SUBROUTINE B()
END B
LIBRARY A
  SUBROUTINE B
  PROGRAM C
END A
`, "SYNTAX ERROR: PROGRAMC")
	testParseError(t, `SUBROUTINE B()
END B
LIBRARY A
  SUBROUTINE B
`, "UNEXPECTED EOF")
	testParseSuccess(t, `USE C
SUBROUTINE B()
  IF A = A
    CALL C.D()
  END IF
END B
LIBRARY A
  SUBROUTINE B
END A
`)
	testParseError(t, `USE C
SUBROUTINE B()
  IF A = A
    CALL D.E()
  END IF
END B
LIBRARY A
  SUBROUTINE B
END A
`, "UNDECLARED LIBRARY: D")
}

func testParseSuccess(t *testing.T, code string) {
	program, err := parse(bufio.NewReader(bytes.NewBufferString(code)))
	if err != nil {
		t.Errorf("parse: err=%s", err.Error())
		return
	}
	buf := bytes.Buffer{}
	err = unparse(&buf, program)
	if err != nil {
		t.Errorf("unparse: err=%s", err.Error())
	}
	if buf.String() != code {
		t.Errorf("parse/unparse mismatch")
	}
}

func testParseError(t *testing.T, code string, errMsg string) {
	_, err := parse(bufio.NewReader(bytes.NewBufferString(code)))
	if err == nil {
		t.Errorf("parse: err=nil")
	} else if err.Error() != errMsg {
		t.Errorf("parse: err=%s", err.Error())
	}
}

func TestReadLine(t *testing.T) {
	buf := bufio.NewReader(bytes.NewBufferString(example))
	expected := []string{
		"USELIBRARY",
		"SUBROUTINEHOME(X,Y)",
		"IFX=Y",
		"RETURN",
		"ELSEIFX>Y",
		"DOI<X",
		"LETX<I",
		"ENDDO",
		"ELSE",
		"CALLLIBRARY.DIE()",
		"ENDIF",
		"LETX>0",
		"ENDHOME",
		"PROGRAMTEST",
		"LETA=0",
		"LETB=0",
		"LETA>B",
		"DOC",
		"LETB>C",
		"CALLHOME(A,C)",
		"EXITC",
		"ENDDO",
		"ENDTEST",
	}
	for _, expectedLine := range expected {
		line, err := readLine(buf)
		if err != nil {
			t.Errorf("Error: %s", err.Error())
		}
		if line != expectedLine {
			t.Errorf("Line: %s, Expected: %s", line, expectedLine)
		}
	}
	line, err := readLine(buf)
	if err == nil {
		t.Errorf("Line: %line", line)
	} else if err != io.EOF {
		t.Errorf("Error: %s", err.Error())
	}
}

func TestTokenizeLine(t *testing.T) {
	for _, data := range []struct {
		line    string
		keyword string
		ok      bool
		tokens  []string
	}{
		{
			line:    "USELIBRARY",
			keyword: "PROGRAM",
			ok:      false,
			tokens:  nil,
		},
		{
			line:    "USELIBRARY",
			keyword: "USE",
			ok:      true,
			tokens:  []string{"LIBRARY"},
		},
		{
			line:    "SUBROUTINEHOME(X,Y)",
			keyword: "SUBROUTINE",
			ok:      true,
			tokens:  []string{"HOME", "(", "X", ",", "Y", ")"},
		},
	} {
		ok, tokens := tokenizeLine(data.line, data.keyword)
		if ok != data.ok {
			t.Errorf("ok: %v", ok)
		}
		if len(tokens) != len(data.tokens) {
			t.Errorf("tokens: %s", data.line)
		} else {
			for i := 0; i < len(tokens); i++ {
				if tokens[i] != data.tokens[i] {
					t.Errorf("tokens: %s, %d", data.line, i)
				}
			}
		}
	}
}

func TestIsIdentifier(t *testing.T) {
	for _, data := range []struct {
		s     string
		id    bool
		idOr0 bool
	}{
		{"0", false, true},
		{"007", true, true},
		{"<", false, false},
		{"AB1", true, true},
	} {
		if isIdentifier(data.s) != data.id {
			t.Errorf("isIdentifier %s %v", data.s, data.id)
		}
		if isIdentifierOr0(data.s) != data.idOr0 {
			t.Errorf("isIdentifierOr0 %s %v", data.s, data.idOr0)
		}
	}
}

func TestUnparse(t *testing.T) {
	program := &program{
		uses: []string{"LIBRARY"},
		subroutines: []subroutine{
			subroutine{
				name:       "HOME",
				parameters: []string{"X", "Y"},
				statements: []statement{
					statement{
						stmtType: stmtIf,
						statements: []statement{
							statement{
								stmtType:   stmtIfEq,
								parameters: []string{"X", "Y"},
								statements: []statement{
									statement{
										stmtType: stmtReturn,
									},
								},
							},
							statement{
								stmtType:   stmtIfEdge,
								parameters: []string{"X", "Y"},
								statements: []statement{
									statement{
										stmtType:   stmtDoEdges,
										parameters: []string{"I", "X"},
										statements: []statement{
											statement{
												stmtType:   stmtLetRmEdge,
												parameters: []string{"X", "I"},
											},
										},
									},
								},
							},
							statement{
								stmtType: stmtElse,
								statements: []statement{
									statement{
										stmtType:   stmtCall,
										parameters: []string{"LIBRARY", "DIE"},
									},
								},
							},
						},
					},
					statement{
						stmtType:   stmtLetMkEdge,
						parameters: []string{"X", "0"},
					},
				},
			},
		},
		name: "TEST",
		statements: []statement{
			statement{
				stmtType:   stmtLetEq,
				parameters: []string{"A", "0"},
			},
			statement{
				stmtType:   stmtLetEq,
				parameters: []string{"B", "0"},
			},
			statement{
				stmtType:   stmtLetMkEdge,
				parameters: []string{"A", "B"},
			},
			statement{
				stmtType:   stmtDoLoop,
				parameters: []string{"C"},
				statements: []statement{
					statement{
						stmtType:   stmtLetMkEdge,
						parameters: []string{"B", "C"},
					},
					statement{
						stmtType:   stmtCall,
						parameters: []string{"", "HOME", "A", "C"},
					},
					statement{
						stmtType:   stmtExit,
						parameters: []string{"C"},
					},
				},
			},
		},
	}
	buf := bytes.Buffer{}
	err := unparse(&buf, program)
	if err != nil {
		t.Errorf("Unexpected error: %s", err.Error())
	}
	if buf.String() != example {
		t.Errorf("Unexpected unparse: %s", buf.String())
	}
}
