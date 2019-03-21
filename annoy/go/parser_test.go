package main

import (
	"bytes"
	"testing"
)

func testParse(filename, src string) ([]Stmt, error) {
	return Parse(NewTokenizer(filename, bytes.NewBufferString(src)))
}

func testParser(t *testing.T, filename, src string, expected []Stmt) {
	stmts, err := testParse(filename, src)
	if err != nil {
		t.Errorf("%s: err=%s", filename, err.Error())
		return
	}
	if len(stmts) != len(expected) {
		t.Errorf("%s: len(stmts)=%d != len(expected)=%d", filename, len(stmts), len(expected))
	}
	for i, stmt := range stmts {
		if i >= len(expected) {
			break
		}
		if !stmtEq(stmt, expected[i]) {
			tok := stmt.StmtFirstToken()
			t.Errorf("%s:%d:%d: stmt[%d] != expected[%d]", filename, tok.Line, tok.Column, i, i)
		}
	}
}

func testParseError(t *testing.T, filename, src string, errMsg string) {
	_, err := testParse(filename, src)
	if err == nil {
		t.Errorf("%s: err=nil != expected=%s", filename, errMsg)
	} else if err.Error() != errMsg {
		t.Errorf("%s: err=%s != expected=%s", filename, err.Error(), errMsg)
	}
}

func stmtEq(s1, s2 Stmt) bool {
	switch stmt := s1.(type) {
	case *StmtAssignment:
		if stmt2, ok := s2.(*StmtAssignment); ok {
			return stmt.Name == stmt2.Name && exprEq(stmt.Expr, stmt2.Expr)
		}
	case *StmtDefineFunc:
		if stmt2, ok := s2.(*StmtDefineFunc); ok {
			if stmt.Name != stmt2.Name || len(stmt.Params) != len(stmt2.Params) || !stmtEq(&stmt.Body, &stmt2.Body) {
				return false
			}
			for i := range stmt.Params {
				if stmt.Params[i] != stmt2.Params[i] {
					return false
				}
			}
			return true
		}
	case *StmtDefineLibFunc:
		if stmt2, ok := s2.(*StmtDefineLibFunc); ok {
			if stmt.Name != stmt2.Name || len(stmt.Params) != len(stmt2.Params) || stmt.Lib != stmt2.Lib {
				return false
			}
			for i := range stmt.Params {
				if stmt.Params[i] != stmt2.Params[i] {
					return false
				}
			}
			return true
		}
	case *StmtExpr:
		if stmt2, ok := s2.(*StmtExpr); ok {
			return exprEq(stmt.Expr, stmt2.Expr)
		}
	case *StmtBlock:
		if stmt2, ok := s2.(*StmtBlock); ok {
			if len(stmt.Stmts) != len(stmt2.Stmts) || !exprEq(stmt.Expr, stmt2.Expr) || stmt.Return != stmt2.Return {
				return false
			}
			for i := range stmt.Stmts {
				if !stmtEq(stmt.Stmts[i], stmt2.Stmts[i]) {
					return false
				}
			}
			return true
		}
	default:
	}
	return false
}

func exprEq(e1, e2 Expr) bool {
	switch expr := e1.(type) {
	case *Expr0:
		if expr2, ok := e2.(*Expr0); ok {
			return expr.Token == expr2.Token
		}
	case *ExprIdentifier:
		if expr2, ok := e2.(*ExprIdentifier); ok {
			return expr.Name == expr2.Name
		}
	case *ExprCallFunction:
		if expr2, ok := e2.(*ExprCallFunction); ok {
			if expr.Name != expr2.Name || len(expr.Args) != len(expr2.Args) {
				return false
			}
			for i := range expr.Args {
				if !exprEq(expr.Args[i], expr2.Args[i]) {
					return false
				}
			}
			return true
		}
	case *ExprBinary:
		if expr2, ok := e2.(*ExprBinary); ok {
			if !exprEq(expr.Left, expr2.Left) || !exprEq(expr.Right, expr2.Right) || expr.Op != expr2.Op {
				return false
			}
			if expr.Block == nil {
				return expr2.Block == nil
			} else if expr2.Block == nil {
				return false
			}
			return stmtEq(expr.Block, expr2.Block)
		}
	case *ExprPop:
		if expr2, ok := e2.(*ExprPop); ok {
			if !exprEq(expr.Expr, expr2.Expr) {
				return false
			}
			if expr.Block == nil {
				return expr2.Block == nil
			} else if expr2.Block == nil {
				return false
			}
			return expr.Block.Name == expr2.Block.Name && stmtEq(&expr.Block.Block, &expr2.Block.Block)
		}
	default:
	}
	return false
}

func TestParser(t *testing.T) {
	testParser(t, "testParser", "a:=0.f(x):={a+x{return 0}.return 0+0}.{f(0+0)}.a", []Stmt{
		&StmtAssignment{
			Name: Token{"a", true, "testParser", 1, 1},
			Expr: &Expr0{Token{"0", false, "testParser", 1, 4}},
		},
		&StmtDefineFunc{
			Name:   Token{"f", true, "testParser", 1, 6},
			Params: []Token{Token{"x", true, "testParser", 1, 8}},
			Body: StmtBlock{
				Token: Token{"{", false, "testParser", 1, 12},
				Stmts: []Stmt{
					&StmtExpr{&ExprBinary{
						Left:  &ExprIdentifier{Token{"a", true, "testParser", 1, 13}},
						Right: &ExprIdentifier{Token{"x", true, "testParser", 1, 15}},
						Op:    Token{"+", false, "testParser", 1, 14},
						Block: &StmtBlock{
							Token:  Token{"{", false, "testParser", 1, 16},
							Stmts:  []Stmt{},
							Expr:   &Expr0{Token{"0", false, "testParser", 1, 24}},
							Return: true,
						},
					}},
				},
				Expr: &ExprBinary{
					Left:  &Expr0{Token{"0", false, "testParser", 1, 34}},
					Right: &Expr0{Token{"0", false, "testParser", 1, 36}},
					Op:    Token{"+", false, "testParser", 1, 35},
					Block: nil,
				},
				Return: true,
			},
		},
		&StmtBlock{
			Token: Token{"{", false, "testParser", 1, 39},
			Stmts: []Stmt{},
			Expr: &ExprCallFunction{
				Name: Token{"f", true, "testParser", 1, 40},
				Args: []Expr{
					&ExprBinary{
						Left:  &Expr0{Token{"0", false, "testParser", 1, 42}},
						Right: &Expr0{Token{"0", false, "testParser", 1, 44}},
						Op:    Token{"+", false, "testParser", 1, 43},
						Block: nil,
					},
				},
			},
		},
		&StmtExpr{
			Expr: &ExprIdentifier{Token{"a", true, "testParser", 1, 48}},
		},
	})
	testParser(t, "testParser0", "a", []Stmt{
		&StmtExpr{&ExprIdentifier{Token{"a", true, "testParser0", 1, 1}}},
	})
	testParser(t, "testParser1", "a.", []Stmt{
		&StmtExpr{&ExprIdentifier{Token{"a", true, "testParser1", 1, 1}}},
	})
	testParser(t, "testParser2", "f():=a", []Stmt{
		&StmtDefineLibFunc{
			Name:   Token{"f", true, "testParser2", 1, 1},
			Params: []Token{},
			Lib:    Token{"a", true, "testParser2", 1, 6},
		},
	})
	testParser(t, "testParser3", "(a).", []Stmt{
		&StmtExpr{&ExprIdentifier{Token{"a", true, "testParser3", 1, 2}}},
	})
	testParser(t, "testParser4", "f().", []Stmt{
		&StmtExpr{
			Expr: &ExprCallFunction{
				Name: Token{"f", true, "testParser4", 1, 1},
				Args: []Expr{},
			},
		},
	})
	testParser(t, "testParser5", "0+(0)", []Stmt{
		&StmtExpr{
			Expr: &ExprBinary{
				Left:  &Expr0{Token{"0", false, "testParser5", 1, 1}},
				Right: &Expr0{Token{"0", false, "testParser5", 1, 4}},
				Op:    Token{"+", false, "testParser5", 1, 2},
				Block: nil,
			},
		},
	})
	testParser(t, "testParser6", "0-", []Stmt{
		&StmtExpr{
			Expr: &ExprPop{
				Expr:  &Expr0{Token{"0", false, "testParser6", 1, 1}},
				Block: nil,
			},
		},
	})
	testParser(t, "testParser7", "0-=0", []Stmt{
		&StmtExpr{
			Expr: &ExprBinary{
				Left: &ExprPop{
					Expr:  &Expr0{Token{"0", false, "testParser7", 1, 1}},
					Block: nil,
				},
				Right: &Expr0{Token{"0", false, "testParser7", 1, 4}},
				Op:    Token{"=", false, "testParser7", 1, 3},
				Block: nil,
			},
		},
	})
	testParser(t, "testParser8", "0-a{a}", []Stmt{
		&StmtExpr{
			Expr: &ExprPop{
				Expr: &Expr0{Token{"0", false, "testParser8", 1, 1}},
				Block: &struct {
					Name  Token
					Block StmtBlock
				}{
					Name: Token{"a", true, "testParser8", 1, 3},
					Block: StmtBlock{
						Token: Token{"{", false, "testParser8", 1, 4},
						Stmts: []Stmt{},
						Expr: &ExprIdentifier{
							Name: Token{"a", true, "testParser8", 1, 5},
						},
					},
				},
			},
		},
	})
	testParser(t, "testParser9", "0=a", []Stmt{
		&StmtExpr{
			Expr: &ExprBinary{
				Left:  &Expr0{Token{"0", false, "testParser9", 1, 1}},
				Right: &ExprIdentifier{Token{"a", true, "testParser9", 1, 3}},
				Op:    Token{"=", false, "testParser9", 1, 2},
				Block: nil,
			},
		},
	})
}

func TestParseError(t *testing.T) {
	testParseError(t, "testEOF", "// comment", "Unexpected EOF")
	testParseError(t, "testError0", "a := 0 :=", "testError0:1:8: Unexpected token: :=")
	testParseError(t, "testError1", "a := 0. :=", "testError1:1:9: Unexpected token: :=")
	testParseError(t, "testError2", "a \":=\" 0.", "testError2:1:3: Unexpected token: :=")
	testParseError(t, "testError3", "{a:=0}", "testError3:1:6: Invalid block")
	testParseError(t, "testError4", "{return a:=0}", "testError4:1:10: Unexpected token: :=")
	testParseError(t, "testError5", "{a.}", "testError5:1:4: Unexpected token: }")
	testParseError(t, "testError6", "{a:=0:=}", "testError6:1:6: Unexpected token: :=")
	testParseError(t, "testError7", "(0+0,0)", "testError7:1:5: Unexpected token: ,")
	testParseError(t, "testError8", "(return 0+0)", "testError8:1:2: Unexpected token: return")
	testParseError(t, "testError9", "f(x,y 0)", "testError9:1:7: Unexpected token: 0")
	testParseError(t, "testError10", "f(x,y)-g()", "testError10:1:9: Unexpected token: (")
	testParseError(t, "testError11", "f(x(),y):=g", "testError11:1:4: Unexpected token: (")
	testParseError(t, "testError12", "f(x,(y)):=g", "testError12:1:5: Unexpected token: (")
	testParseError(t, "testError13", "f(x,x):=g", "testError13:1:5: Duplicate parameter name: x")
	testParseError(t, "testError14", "f(x,y):={}", "testError14:1:10: Unexpected token: }")
	testParseError(t, "testError15", "f(x,y):=+", "testError15:1:9: Unexpected token: +")
	testParseError(t, "testError16", "f(x,y):={return", "Unexpected EOF")
}
