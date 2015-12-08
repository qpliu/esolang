package main

import (
	"strings"
	"testing"
)

func testParse(t *testing.T, label, input, expectedErr string, expectedFuncs, expectedTypes []string) *Ast {
	tokens := make(chan Token)
	go func() {
		Tokenize(strings.NewReader(input), tokens)
		close(tokens)
	}()
	ast, err := Parse(tokens)
	if err == nil && expectedErr != "" {
		t.Errorf("%s: Failed to get expected error `%s'", label, expectedErr)
	} else if err != nil && expectedErr != err.Error() {
		t.Errorf("%s: Expected error `%s', got error `%s'", label, expectedErr, err.Error())
	}
	if err != nil {
		return ast
	}
	if len(expectedFuncs) != len(ast.Funcs) {
		t.Errorf("%s: Expected %d func(s), got %d", label, len(expectedFuncs), len(ast.Funcs))
	}
	if len(expectedTypes) != len(ast.Types) {
		t.Errorf("%s: Expected %d type(s), got %d", label, len(expectedTypes), len(ast.Types))
	}
	for _, name := range expectedFuncs {
		if funcDecl, ok := ast.Funcs[name]; !ok {
			t.Errorf("%s: Did not find expected func `%s'", label, name)
		} else if funcDecl.Name != name {
			t.Errorf("%s: Function name mismatch `%s' != `%s'", label, funcDecl.Name, name)
		}
	}
	for _, name := range expectedTypes {
		if typeDecl, ok := ast.Types[name]; !ok {
			t.Errorf("%s: Did not find expected type `%s'", label, name)
		} else if typeDecl.Name != name {
			t.Errorf("%s: Type name mismatch `%s' != `%s'", label, typeDecl.Name, name)
		}
	}
	return ast
}

func checkTypeFields(t *testing.T, label string, typeDecl *Type, fields []*Var) {
	if len(typeDecl.Fields) != len(fields) {
		t.Errorf("%s: Expected %d field(s), got %d", label, len(fields), len(typeDecl.Fields))
	}
	for i, field := range typeDecl.Fields {
		if field.Name != fields[i].Name {
			t.Errorf("%s: Expected field name `%s', got `%s'", label, fields[i].Name, field.Name)
		}
		if field.TypeName != fields[i].TypeName {
			t.Errorf("%s: Expected field type name `%s', got `%s'", label, fields[i].TypeName, field.TypeName)
		}
	}
}

func checkFuncParams(t *testing.T, label string, funcDecl *Func, params []*Var, returnType string) {
	if len(funcDecl.Params) != len(params) {
		t.Errorf("%s: Expected %d param(s), got %d", label, len(params), len(funcDecl.Params))
	}
	for i, param := range funcDecl.Params {
		if param.Name != params[i].Name {
			t.Errorf("%s: Expected parameter name `%s', got `%s'", label, params[i].Name, param.Name)
		}
		if param.TypeName != params[i].TypeName {
			t.Errorf("%s: Expected parameter type name `%s', got `%s'", label, params[i].TypeName, param.TypeName)
		}
	}
	if returnType != funcDecl.TypeName {
		t.Errorf("%s: Expected return type name `%s', got `%s'", label, returnType, funcDecl.TypeName)
	}
}

func checkStmt(t *testing.T, label string, stmt, expected Stmt) {
	switch ex := expected.(type) {
	case nil:
		if stmt != nil {
			t.Errorf("%s: Did not get expected nil statement", label)
		}
	case *StmtBlock:
		actual, ok := stmt.(*StmtBlock)
		if !ok {
			t.Errorf("%s: Did not get expected block statement", label)
		}
		if len(ex.Stmts) != len(actual.Stmts) {
			t.Errorf("%s: Expected %d statement(s) in block statement, got %d", label, len(ex.Stmts), len(actual.Stmts))
		}
		for i, st := range actual.Stmts {
			checkStmt(t, label+"/StmtBlock", st, ex.Stmts[i])
		}
	case *StmtVar:
		actual, ok := stmt.(*StmtVar)
		if !ok {
			t.Errorf("%s: Did not get expected var statement declaring %s %s", label, ex.Name, ex.TypeName)
		}
		if ex.Name != actual.Name || ex.TypeName != actual.TypeName {
			t.Errorf("%s: Expected var %s %s, got var %s %s", ex.Name, ex.TypeName, actual.Name, actual.TypeName)
		}
		checkExpr(t, label, actual.Expr, ex.Expr)
	case *StmtIf:
		actual, ok := stmt.(*StmtIf)
		if !ok {
			t.Errorf("%s: Did not get expected if statement", label)
		}
		checkExpr(t, label, actual.Expr, ex.Expr)
		checkStmt(t, label, actual.Stmts, ex.Stmts)
		if actual.ElseIf != nil && ex.ElseIf != nil {
			checkStmt(t, label, actual.ElseIf, ex.ElseIf)
			if actual.Else != nil || ex.Else != nil {
				panic("StmtIf with both ElseIf and Else")
			}
		} else if ex.ElseIf != nil {
			t.Errorf("%s: Did not get expected else if statement", label)
		} else if actual.ElseIf != nil {
			t.Errorf("%s: Got unexpected else if statement", label)
		}
		if actual.Else != nil && ex.Else != nil {
			checkStmt(t, label, actual.Else, ex.Else)
		} else if ex.Else != nil {
			t.Errorf("%s: Did not get expected else statement", label)
		} else if actual.Else != nil {
			t.Errorf("%s: Got unexpected else statement", label)
		}
	case *StmtFor:
		actual, ok := stmt.(*StmtFor)
		if !ok {
			t.Errorf("%s: Did not get expected for statement", label)
		}
		if ex.Label != actual.Label {
			t.Errorf("%s: Expected for label %s, got %s", ex.Label, actual.Label)
		}
		checkStmt(t, label, actual.Stmts, ex.Stmts)
	case *StmtBreak:
		actual, ok := stmt.(*StmtBreak)
		if !ok {
			t.Errorf("%s: Did not get expected break statement", label)
		}
		if ex.Label != actual.Label {
			t.Errorf("%s: Expected break label %s, got %s", ex.Label, actual.Label)
		}
	case *StmtReturn:
		actual, ok := stmt.(*StmtReturn)
		if !ok {
			t.Errorf("%s: Did not get expected return statement", label)
		}
		checkExpr(t, label, actual.Expr, ex.Expr)
	case *StmtSetClear:
		actual, ok := stmt.(*StmtSetClear)
		if !ok || ex.Value != actual.Value {
			t.Errorf("%s: Did not get expected set/clear statement", label)
		}
		checkExpr(t, label, actual.Expr, ex.Expr)
	case *StmtAssign:
		actual, ok := stmt.(*StmtAssign)
		if !ok {
			t.Errorf("%s: Did not get expected assignment statement", label)
		}
		checkExpr(t, label+"/LValue", actual.LValue, ex.LValue)
		checkExpr(t, label, actual.Expr, ex.Expr)
	case *StmtExpr:
		actual, ok := stmt.(*StmtExpr)
		if !ok {
			t.Errorf("%s: Did not get expected expression statement", label)
		}
		checkExpr(t, label, actual.Expr, ex.Expr)
	default:
		panic("Unrecognized statement type")
	}
}

func checkExpr(t *testing.T, label string, expr, expected Expr) {
	switch ex := expected.(type) {
	case nil:
		if expr != nil {
			t.Errorf("%s: Did not get expected nil expression", label)
		}
	case *ExprVar:
		actual, ok := expr.(*ExprVar)
		if !ok {
			t.Errorf("%s: Did not get expected local variable expression `%s'", label, ex.Name)
		}
		if ex.Name != actual.Name {
			t.Errorf("%s: Expected local variable name `%s', got `%s'", label, ex.Name, actual.Name)
		}
	case *ExprField:
		actual, ok := expr.(*ExprField)
		if !ok {
			t.Errorf("%s: Did not get expected field reference expression `.%s'", label, ex.Name)
		}
		if ex.Name != actual.Name {
			t.Errorf("%s: Expected field name `%s', got `%s'", label, ex.Name, actual.Name)
		}
		checkExpr(t, label, actual.Expr, ex.Expr)
	case *ExprFunc:
		actual, ok := expr.(*ExprFunc)
		if !ok {
			t.Errorf("%s: Did not get expected function call expression `%s'", label, ex.Name)
		}
		if ex.Name != actual.Name {
			t.Errorf("%s: Expected function name `%s', got `%s'", label, ex.Name, actual.Name)
		}
		if len(ex.Params) != len(actual.Params) {
			t.Errorf("%s: Expected %d parameters for call of function `%s', got `%s'", label, len(ex.Params), ex.Name, len(actual.Params))
		}
		for i, param := range actual.Params {
			checkExpr(t, label+"/"+ex.Name+"()", param, ex.Params[i])
		}
	default:
		panic("Unrecognized expression type")
	}
}

func TestParse(t *testing.T) {
	testParse(t, "empty", "", "", []string{}, []string{})

	testParse(t, "immdiate error", "foo", "Expected 'import', 'type', or 'func', got:foo", []string{}, []string{})

	ast := testParse(t, "single type", `
type a {
  a, b
  c d
}
`, "", []string{}, []string{"a"})
	checkTypeFields(t, "single type", ast.Types["a"], []*Var{
		&Var{Name: "a", TypeName: ""},
		&Var{Name: "b", TypeName: ""},
		&Var{Name: "c", TypeName: "d"},
	})

	ast = testParse(t, "import func", `
import func &(a, b bit) bit
import func +(a, b bit) bit+carry
import func putByte(b byte)
import func getByte(b byte)
`, "", []string{"&", "+", "putByte", "getByte"}, []string{})
	checkFuncParams(t, "import func &", ast.Funcs["&"], []*Var{
		&Var{Name: "a", TypeName: "bit"},
		&Var{Name: "b", TypeName: "bit"},
	}, "bit")
	checkFuncParams(t, "import func +", ast.Funcs["+"], []*Var{
		&Var{Name: "a", TypeName: "bit"},
		&Var{Name: "b", TypeName: "bit"},
	}, "bit+carry")
	checkFuncParams(t, "import func putByte", ast.Funcs["putByte"], []*Var{
		&Var{Name: "b", TypeName: "byte"},
	}, "")
	checkFuncParams(t, "import func getByte", ast.Funcs["getByte"], []*Var{
		&Var{Name: "b", TypeName: "byte"},
	}, "")

	ast = testParse(t, "import func and type", "import func pause(); type a { a; b c; }", "", []string{"pause"}, []string{"a"})
	checkTypeFields(t, "import func and type", ast.Types["a"], []*Var{
		&Var{Name: "a", TypeName: ""},
		&Var{Name: "b", TypeName: "c"},
	})
	checkFuncParams(t, "import func and type", ast.Funcs["pause"], []*Var{}, "")

	ast = testParse(t, "empty func", "func main() {}", "", []string{"main"}, []string{})
	checkFuncParams(t, "empty func main", ast.Funcs["main"], []*Var{}, "")
	checkStmt(t, "empty func main", ast.Funcs["main"].Body, &StmtBlock{})

	ast = testParse(t, "various statements", `
type t {
    b
}

func main() {
    for label {
        for {
            break label
        }
        break
    }
    var a t
    if a.b {
        var b t = a
        main()
        return
    }
    var b t = a
    set a.b
    clear b.b
    a.b = b.b
    if a.b {
    } else if b.b {
    } else {
    }
    {
    }
}
`, "", []string{"main"}, []string{"t"})
	checkFuncParams(t, "various statements main", ast.Funcs["main"], []*Var{}, "")
	checkTypeFields(t, "various statements type", ast.Types["t"], []*Var{&Var{Name: "b", TypeName: ""}})
	checkStmt(t, "various statements", ast.Funcs["main"].Body, &StmtBlock{
		Stmts: []Stmt{
			&StmtFor{
				Label: "label",
				Stmts: &StmtBlock{
					Stmts: []Stmt{
						&StmtFor{
							Stmts: &StmtBlock{
								Stmts: []Stmt{
									&StmtBreak{Label: "label"},
								},
							},
						},
						&StmtBreak{},
					},
				},
			},
			&StmtVar{Name: "a", TypeName: "t"},
			&StmtIf{
				Expr: &ExprField{
					Name: "b",
					Expr: &ExprVar{Name: "a"},
				},
				Stmts: &StmtBlock{
					Stmts: []Stmt{
						&StmtVar{
							Name:     "b",
							TypeName: "t",
							Expr:     &ExprVar{Name: "a"},
						},
						&StmtExpr{
							Expr: &ExprFunc{Name: "main"},
						},
						&StmtReturn{},
					},
				},
			},
			&StmtVar{
				Name:     "b",
				TypeName: "t",
				Expr:     &ExprVar{Name: "a"},
			},
			&StmtSetClear{
				Value: true,
				Expr:  &ExprField{Name: "b", Expr: &ExprVar{Name: "a"}},
			},
			&StmtSetClear{
				Value: false,
				Expr:  &ExprField{Name: "b", Expr: &ExprVar{Name: "b"}},
			},
			&StmtAssign{
				LValue: &ExprField{Name: "b", Expr: &ExprVar{Name: "a"}},
				Expr:   &ExprField{Name: "b", Expr: &ExprVar{Name: "b"}},
			},
			&StmtIf{
				Expr:  &ExprField{Name: "b", Expr: &ExprVar{Name: "a"}},
				Stmts: &StmtBlock{},
				ElseIf: &StmtIf{
					Expr:  &ExprField{Name: "b", Expr: &ExprVar{Name: "b"}},
					Stmts: &StmtBlock{},
					Else:  &StmtBlock{},
				},
			},
			&StmtBlock{},
		},
	})
}
