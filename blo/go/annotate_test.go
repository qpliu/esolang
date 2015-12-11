package main

import (
	"strings"
	"testing"
)

func testAnnotate(t *testing.T, label, input, expectedErr string) *Ast {
	tokens := make(chan Token)
	go func() {
		Tokenize("(stdin)", strings.NewReader(input), tokens)
		close(tokens)
	}()
	if ast, err := Parse(tokens); err != nil {
		t.Errorf("%s: Unexpected parse error: %s", label, err.Error())
	} else if err := ast.Annotate(); err != nil {
		if expectedErr != err.Error() {
			t.Errorf("%s: Expected error `%s', got error `%s'", label, expectedErr, err.Error())
		}
	} else if expectedErr != "" {
		t.Errorf("%s: Failed to get expected error `%s'", label, expectedErr)
	} else {
		return ast
	}
	return nil
}

func checkAnnotatedType(t *testing.T, label string, typeDecl *Type, expectedFields []*Var, expectedBitSize, expectedOpaqueSize int) {
	if len(typeDecl.Fields) != len(expectedFields) {
		t.Errorf("%s: Expected %d fields, got %d", label, len(expectedFields), len(typeDecl.Fields))
	}
	for i, field := range typeDecl.Fields {
		if field.Name != expectedFields[i].Name || field.TypeName != expectedFields[i].TypeName || field.Type != expectedFields[i].Type {
			t.Errorf("%s: Expected field %d `%s %s', got `%s %s'", label, i, expectedFields[i].Name, expectedFields[i].TypeName, field.Name, field.TypeName)
		}
	}
	if expectedBitSize != typeDecl.BitSize() {
		t.Errorf("%s: Expected bit size %d, got %d", label, expectedBitSize, typeDecl.BitSize())
	}
	if expectedOpaqueSize != typeDecl.OpaqueSize() {
		t.Errorf("%s: Expected opaque size %d, got %d", label, expectedOpaqueSize, typeDecl.OpaqueSize())
	}
}

func checkAnnotatedFuncDecl(t *testing.T, label string, funcDecl *Func, expectedType *Type, expectedParams []*Var) {
	if funcDecl.Type != expectedType {
		if expectedType == nil {
			t.Errorf("%s: Function `%s' expected no return type, got `%s'", label, funcDecl.Type.Name)
		} else if funcDecl.Type == nil {
			t.Errorf("%s: Function `%s' expected to return type `%s', got no return type", label, expectedType.Name)
		} else {
			t.Errorf("%s: Function `%s' expected to return type `%s', got `%s'", label, expectedType.Name, funcDecl.Type.Name)
		}
	}
	if len(funcDecl.Params) != len(expectedParams) {
		t.Errorf("%s: Expected %d parameters, got %d", label, len(expectedParams), len(funcDecl.Params))
	}
	for i, param := range funcDecl.Params {
		if param.Name != expectedParams[i].Name || param.TypeName != expectedParams[i].TypeName || param.Type != expectedParams[i].Type {
			t.Errorf("%s: Expected field %d `%s %s', got `%s %s'", label, i, expectedParams[i].Name, expectedParams[i].TypeName, param.Name, param.TypeName)
		}
	}
}

func checkAnnotatedStmt(t *testing.T, label string, stmt, expected Stmt) {
	switch st := expected.(type) {
	case nil:
		panic("Unexpected nil")
	case *StmtBlock:
		if s, ok := stmt.(*StmtBlock); !ok {
			t.Errorf("%s: %s: Expected block statement", label, stmt.Location().String())
		} else if st == nil && s == nil {
		} else if st == nil {
			t.Errorf("%s: %s: Unexpected block statment", label, s.Location().String())
		} else if len(st.Stmts) != len(s.Stmts) {
			t.Errorf("%s: %s: Expected block statement size %d, got %d ", label, stmt.Location().String())
		} else {
			for i, stmt := range s.Stmts {
				checkAnnotatedStmt(t, label, stmt, st.Stmts[i])
			}
		}
	case *StmtVar:
		if s, ok := stmt.(*StmtVar); !ok {
			t.Errorf("%s: %s: Expected var statement", label, stmt.Location().String())
		} else {
			checkAnnotatedExpr(t, label, s.Expr, st.Expr)
		}
	case *StmtIf:
		if s, ok := stmt.(*StmtIf); !ok {
			t.Errorf("%s: %s: Expected if statement", label, stmt.Location().String())
		} else if s == nil && st == nil {
		} else {
			checkAnnotatedExpr(t, label, s.Expr, st.Expr)
			checkAnnotatedStmt(t, label, s.Stmts, st.Stmts)
			checkAnnotatedStmt(t, label, s.ElseIf, st.ElseIf)
			checkAnnotatedStmt(t, label, s.Else, st.Else)
		}
	case *StmtFor:
		if s, ok := stmt.(*StmtFor); !ok {
			t.Errorf("%s: %s: Expected for statement", label, stmt.Location().String())
		} else {
			checkAnnotatedStmt(t, label, s.Stmts, st.Stmts)
		}
	case *StmtBreak:
		if _, ok := stmt.(*StmtBreak); !ok {
			t.Errorf("%s: %s: Expected break statement", label, stmt.Location().String())
		}
	case *StmtReturn:
		if s, ok := stmt.(*StmtReturn); !ok {
			t.Errorf("%s: %s: Expected return statement", label, stmt.Location().String())
		} else {
			checkAnnotatedExpr(t, label, s.Expr, st.Expr)
		}
	case *StmtSetClear:
		if s, ok := stmt.(*StmtSetClear); !ok {
			t.Errorf("%s: %s: Expected set/clear statement", label, stmt.Location().String())
		} else {
			checkAnnotatedExpr(t, label, s.Expr, st.Expr)
		}
	case *StmtExpr:
		if s, ok := stmt.(*StmtExpr); !ok {
			t.Errorf("%s: %s: Expected expression statement", label, stmt.Location().String())
		} else {
			checkAnnotatedExpr(t, label, s.Expr, st.Expr)
		}
	case *StmtAssign:
		if s, ok := stmt.(*StmtAssign); !ok {
			t.Errorf("%s: %s: Expected assignment statement", label, stmt.Location().String())
		} else {
			checkAnnotatedExpr(t, label, s.LValue, st.LValue)
			checkAnnotatedExpr(t, label, s.Expr, st.Expr)
		}
	}
}

func checkAnnotatedExpr(t *testing.T, label string, expr, expected Expr) {
	switch ex := expected.(type) {
	case nil:
		if expr != nil {
			t.Errorf("%s: %s: Unexpected expression", label, expr.Location().String())
		}
	case *ExprVar:
		if e, ok := expr.(*ExprVar); !ok {
			t.Errorf("%s: %s: Expected local variable %s %s", label, expr.Location().String(), ex.Name, ex.Var.TypeName)
		} else if e.Name != ex.Name || e.Var.Name != ex.Var.Name || e.Var.TypeName != ex.Var.TypeName || e.Var.Type != ex.Var.Type {
			t.Errorf("%s: %s: Expected local variable %s %s, got %s %s", label, expr.Location().String(), ex.Name, ex.Var.TypeName, e.Name, e.Var.TypeName)
		}
	case *ExprField:
		if e, ok := expr.(*ExprField); !ok {
			t.Errorf("%s: %s: Expected field reference .%s", label, expr.Location().String(), ex.Name)
		} else if e.Name != ex.Name {
			t.Errorf("%s: %s: Expected field reference .%s, got .%s", label, expr.Location().String(), ex.Name, e.Name)
		} else {
			checkAnnotatedExpr(t, label, e.Expr, ex.Expr)
		}
	case *ExprFunc:
		if e, ok := expr.(*ExprFunc); !ok {
			t.Errorf("%s: %s: Expected function call %s", label, expr.Location().String(), ex.Name)
		} else if e.Name != ex.Name || e.Func != ex.Func || len(e.Params) != len(ex.Params) {
			t.Errorf("%s: %s: Expected function call %s(%d-ary), got %s(%d-ary)", label, expr.Location().String(), ex.Name, len(ex.Params), e.Name, len(ex.Params))
		} else {
			for i, param := range e.Params {
				checkAnnotatedExpr(t, label, param, ex.Params[i])
			}
		}
	}
}

func checkAnnotatedFlow(t *testing.T, label string, body Stmt, expected map[string]string) {
	nexts := make(map[string]string)
	checkNext := func(stmt, next Stmt) {
		loc := stmt.Location().String()
		var nextLoc string
		if next != nil {
			nextLoc = next.Location().String()
		}
		nexts[loc] = nextLoc

		if expectedNext, ok := expected[loc]; !ok || expectedNext != nextLoc {
			t.Errorf("%s: Expected flow from %s to %s, got %s", label, loc, expected[loc], nextLoc)
		}
	}
	var checkStmt func(Stmt)

	checkStmt = func(stmt Stmt) {
		switch st := stmt.(type) {
		case nil:
		case *StmtBlock:
			if st == nil {
				return
			}
			for _, s := range st.Stmts {
				checkStmt(s)
			}
			if st.Next != nil {
				checkNext(st, st.Next)
			}
		case *StmtVar:
			checkNext(st, st.Next)
		case *StmtIf:
			if st == nil {
				return
			}
			checkStmt(st.Stmts)
			checkStmt(st.ElseIf)
			checkStmt(st.Else)
			if st.Next != nil {
				checkNext(st, st.Next)
			}
		case *StmtFor:
			checkStmt(st.Stmts)
		case *StmtBreak:
			checkNext(st, st.Next)
		case *StmtReturn:
			checkNext(st, nil)
		case *StmtSetClear:
			checkNext(st, st.Next)
		case *StmtAssign:
			checkNext(st, st.Next)
		case *StmtExpr:
			checkNext(st, st.Next)
		default:
			panic("Unknown statement type")
		}
	}
	checkStmt(body)
	for loc, nextLoc := range expected {
		if _, ok := nexts[loc]; !ok {
			t.Errorf("%s: Expected flow from %s to %s", label, loc, nextLoc)
		}
	}
}

func TestAnnotate(t *testing.T) {
	ast := testAnnotate(t, "single type", `type a { a, b }`, "")
	checkAnnotatedType(t, "single type", ast.Types["a"], []*Var{
		&Var{Name: "a", TypeName: "", Type: nil},
		&Var{Name: "b", TypeName: "", Type: nil},
	}, 2, 0)

	testAnnotate(t, "single recursive type", `type a { a a }`, "(stdin):1:1: Type contains recursive field of type 'a'")

	ast = testAnnotate(t, "embedded type", `type a { a } type b { 1, 2 a }`, "")
	checkAnnotatedType(t, "embedded type:a", ast.Types["a"], []*Var{
		&Var{Name: "a", TypeName: "", Type: nil},
	}, 1, 0)
	checkAnnotatedType(t, "embedded type:b", ast.Types["b"], []*Var{
		&Var{Name: "1", TypeName: "a", Type: ast.Types["a"]},
		&Var{Name: "2", TypeName: "a", Type: ast.Types["a"]},
	}, 2, 0)

	ast = testAnnotate(t, "func no params", `func main() {}`, "")
	checkAnnotatedFuncDecl(t, "func no params:main", ast.Funcs["main"], nil, []*Var{})

	ast = testAnnotate(t, "func with type", `
type a {
  a
}
func a(a a) a {
  return a
}
`, "")
	checkAnnotatedFuncDecl(t, "func with type", ast.Funcs["a"], ast.Types["a"], []*Var{&Var{Name: "a", TypeName: "a", Type: ast.Types["a"]}})

	ast = testAnnotate(t, "func exprs", `
type a {
  a
}
type b {
  1, 2 a
  b
}
func a(b b) a {
  var 1 a = b.1
  var 2 a = b.2
  if b.b {
    return 1
  } else {
    var c b
    c.1 = b.2
    c.2 = b.1
    set c.b
    return a(c)
  }
}
`, "")
	checkAnnotatedFuncDecl(t, "func exprs", ast.Funcs["a"], ast.Types["a"], []*Var{&Var{Name: "b", TypeName: "b", Type: ast.Types["b"]}})
	checkAnnotatedStmt(t, "func exprs", ast.Funcs["a"].Body, &StmtBlock{
		Stmts: []Stmt{
			&StmtVar{
				Var: Var{Name: "1", TypeName: "a", Type: ast.Types["a"]},
				Expr: &ExprField{
					Name: "1",
					Expr: &ExprVar{
						Name: "b",
						Var:  &Var{Name: "b", TypeName: "b", Type: ast.Types["b"]},
					},
				},
			},
			&StmtVar{
				Var: Var{Name: "2", TypeName: "a", Type: ast.Types["a"]},
				Expr: &ExprField{
					Name: "2",
					Expr: &ExprVar{
						Name: "b",
						Var:  &Var{Name: "b", TypeName: "b", Type: ast.Types["b"]},
					},
				},
			},
			&StmtIf{
				Expr: &ExprField{
					Name: "b",
					Expr: &ExprVar{
						Name: "b",
						Var:  &Var{Name: "b", TypeName: "b", Type: ast.Types["b"]},
					},
				},
				Stmts: &StmtBlock{
					Stmts: []Stmt{
						&StmtReturn{
							Expr: &ExprVar{
								Name: "1",
								Var:  &Var{Name: "1", TypeName: "a", Type: ast.Types["a"]},
							},
						},
					},
				},
				Else: &StmtBlock{
					Stmts: []Stmt{
						&StmtVar{
							Var: Var{Name: "c", TypeName: "b", Type: ast.Types["b"]},
						},
						&StmtAssign{
							LValue: &ExprField{
								Name: "1",
								Expr: &ExprVar{
									Name: "c",
									Var:  &Var{Name: "c", TypeName: "b", Type: ast.Types["b"]},
								},
							},
							Expr: &ExprField{
								Name: "2",
								Expr: &ExprVar{
									Name: "b",
									Var:  &Var{Name: "b", TypeName: "b", Type: ast.Types["b"]},
								},
							},
						},
						&StmtAssign{
							LValue: &ExprField{
								Name: "2",
								Expr: &ExprVar{
									Name: "c",
									Var:  &Var{Name: "c", TypeName: "b", Type: ast.Types["b"]},
								},
							},
							Expr: &ExprField{
								Name: "1",
								Expr: &ExprVar{
									Name: "b",
									Var:  &Var{Name: "b", TypeName: "b", Type: ast.Types["b"]},
								},
							},
						},
						&StmtSetClear{
							Value: true,
							Expr: &ExprField{
								Name: "b",
								Expr: &ExprVar{
									Name: "c",
									Var:  &Var{Name: "c", TypeName: "b", Type: ast.Types["b"]},
								},
							},
						},
						&StmtReturn{
							Expr: &ExprFunc{
								Name: "a",
								Params: []Expr{
									&ExprVar{
										Name: "c",
										Var:  &Var{Name: "c", TypeName: "b", Type: ast.Types["b"]},
									},
								},
								Func: ast.Funcs["a"],
							},
						},
					},
				},
			},
		},
	})
	checkAnnotatedFlow(t, "func flow", ast.Funcs["a"].Body, map[string]string{
		"(stdin):10:3": "(stdin):11:3",
		"(stdin):11:3": "(stdin):12:3",
		"(stdin):13:5": "",
		"(stdin):15:5": "(stdin):16:5",
		"(stdin):16:5": "(stdin):17:5",
		"(stdin):17:5": "(stdin):18:5",
		"(stdin):18:5": "(stdin):19:5",
		"(stdin):19:5": "",
	})

	testAnnotate(t, "type error in var", `
type a {
  a
}
type b {
}
func a(a a) a {
  var b b = a
  return a
}
`, "(stdin):8:3: Initializer type mismatch")

	testAnnotate(t, "type error in assign", `
type a {
  a
}
type b {
}
func a(a a) a {
  var b b
  a = b
  return a
}
`, "(stdin):9:3: type mismatch in assignment statement")

	testAnnotate(t, "type error if return", `
type a {
  a
}
type b {
}
func a(a a) a {
  var b b
  return b
}
`, "(stdin):9:3: return expression does not match the functions return type")

	testAnnotate(t, "type error in if", `
type a {
  a
}
type b {
}
func a(a a) a {
  if a {
  } else {
  }
  return a
}
`, "(stdin):8:6: the type of the condition expression in the if statement is not a bit field")

	testAnnotate(t, "type error in function call 1", `
type a {
  a
}
type b {
}
func a(a a) a {
  if a.a {
    var b b
    return a(b)
  }
}
`, "(stdin):10:12: Type mismatch for an argument of function 'a'")

	testAnnotate(t, "type error in function call 2", `
type a {
  a
}
type b {
}
func a(a a) a {
  return b(a)
}
func b(a a) b {
  var b b
  return b
}
`, "(stdin):8:3: return expression does not match the functions return type")

	testAnnotate(t, "arity error in function call 1", `
type a {
  a
}
type b {
}
func a(a a) a {
  return a()
}
`, "(stdin):8:10: Wrong number of arguments to function 'a'")

	testAnnotate(t, "arity error in function call 2", `
type a {
  a
}
type b {
}
func a(a a) a {
  return a(a, a)
}
`, "(stdin):8:10: Wrong number of arguments to function 'a'")

	testAnnotate(t, "type error in set", `
type a {
  a
}
func a(a a) a {
  set a
  return a
}
`, "(stdin):6:3: set/clear expression is not a bit field")

	testAnnotate(t, "missing return", `
type a {
  a
}
func a(a a) a {
  for {
    set a.a
    break
  }
}
`, "(stdin):5:1: Missing required return statement")

	testAnnotate(t, "unreachable statement", `
type a {
  a
}
func a(a a) a {
  for {
    set a.a
  }
  return a
}
`, "(stdin):9:3: Unreachable")

	testAnnotate(t, "invalid break", `
type a {
  a
}
func a(a a) a {
  break
}
`, "(stdin):6:3: Invalid break statement")

	testAnnotate(t, "unreachable 2", `
type a {
  a
}
func a(a a) {
  for loop {
    if a.a {
      for {
        break loop
      }
      clear a.a
    }
  }
}
`, "(stdin):11:7: Unreachable")

	ast = testAnnotate(t, "break flow", `
type a { a }
func main() {
  var a a
  for loop {
    if a.a {
      break
    } else {
      for {
        break loop
      }
    }
  }
  if a.a {
    for loop {
      if a.a {
        for {
          if a.a {
            return
          }
          break loop
        }
      }
    }
  } else {
    return
  }
  for loop {
    break loop
  }
}
`, "")
	checkAnnotatedFlow(t, "func flow", ast.Funcs["main"].Body, map[string]string{
		"(stdin):4:3":   "(stdin):5:3",
		"(stdin):7:7":   "(stdin):14:3",
		"(stdin):10:9":  "(stdin):14:3",
		"(stdin):19:13": "",
		"(stdin):18:11": "(stdin):21:11",
		"(stdin):21:11": "(stdin):28:3",
		"(stdin):26:5":  "",
		"(stdin):29:5":  "",
	})
}
