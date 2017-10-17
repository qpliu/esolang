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
						parameters: []string{"HOME", "A", "C"},
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
