package main

// Initial implementation
// Does not do any static analysis
// Does not do tail call elimination

import (
	"io"
)

type scope struct {
	parent    *scope
	caller    *scope
	idents    map[string]ident
	functions map[string]function

	stmts []Stmt
	expr  Expr
	index int
}

type ident struct {
	version int
	value   *Value
}

type function struct {
	token         Token
	definingScope *scope
	params        []string
	body          *StmtBlock
	lib           LibFunc
}

func Interp(r io.Reader, w io.Writer, stmts []Stmt) error {
	_, _, err := interp(r, w, &scope{
		parent:    nil,
		caller:    nil,
		idents:    make(map[string]ident),
		functions: make(map[string]function),
		stmts:     stmts,
		expr:      nil,
		index:     0,
	})
	return err
}

func interp(r io.Reader, w io.Writer, s *scope) (*Value, bool, error) {
	for {
		if s.index >= len(s.stmts) {
			if s.expr == nil {
				return nil, false, nil
			}
			return eval(r, w, s, s.expr)
		}
		switch stmt := s.stmts[s.index].(type) {
		case *StmtAssignment:
			val, isReturn, err := eval(r, w, s, stmt.Expr)
			if isReturn || err != nil {
				return val, isReturn, err
			}
			s.idents[stmt.Name.Value] = ident{version: val.Version(), value: val}
		case *StmtDefineFunc:
			params := []string{}
			for _, param := range stmt.Params {
				params = append(params, param.Value)
			}
			s.functions[stmt.Name.Value] = function{token: stmt.StmtFirstToken(), definingScope: s, params: params, body: &stmt.Body, lib: nil}
		case *StmtDefineLibFunc:
			libFunc, err := GetLibFunc(stmt.Lib, len(stmt.Params))
			if err != nil {
				return nil, false, err
			}
			params := []string{}
			for _, param := range stmt.Params {
				params = append(params, param.Value)
			}
			s.functions[stmt.Name.Value] = function{token: stmt.StmtFirstToken(), definingScope: s, params: params, body: nil, lib: libFunc}
		case *StmtExpr:
			val, isReturn, err := eval(r, w, s, stmt.Expr)
			if isReturn || err != nil {
				return val, isReturn, err
			}
		case *StmtBlock:
			val, isReturn, err := interp(r, w, &scope{
				parent:    s,
				caller:    s.caller,
				idents:    make(map[string]ident),
				functions: make(map[string]function),
				stmts:     stmt.Stmts,
				expr:      stmt.Expr,
				index:     0,
			})
			if isReturn || stmt.Return || err != nil {
				return val, isReturn || stmt.Return, err
			}
		default:
			panic("Unknown statement")
		}
		s.index++
	}
}

func eval(r io.Reader, w io.Writer, s *scope, expr Expr) (*Value, bool, error) {
	switch ex := expr.(type) {
	case *Expr0:
		return &Value{}, false, nil
	case *ExprIdentifier:
		sc := s
		for {
			ident, ok := sc.idents[ex.Name.Value]
			if ok {
				if ident.version != ident.value.Version() {
					return nil, false, ex.Name.Errorf("Identifier %s refers to an unaccessible value", ex.Name.Value)
				}
				return ident.value, false, nil
			}
			sc = sc.parent
			if sc == nil {
				return nil, false, ex.Name.Errorf("Undefined identifier %s", ex.Name.Value)
			}
		}
	case *ExprCallFunction:
		var fn function
		sc := s
		for {
			ok := false
			fn, ok = sc.functions[ex.Name.Value]
			if ok {
				if len(fn.params) != len(ex.Args) {
					return nil, false, ex.Name.Errorf("Function %s called with %d argument(s), expected %d argument(s)", ex.Name.Value, len(ex.Args), len(fn.params))
				}
				break
			}
			sc = sc.parent
			if sc == nil {
				return nil, false, ex.Name.Errorf("Undefined function %s", ex.Name.Value)
			}
		}
		if fn.body != nil {
			sc = &scope{
				parent:    fn.definingScope,
				caller:    s,
				idents:    make(map[string]ident),
				functions: make(map[string]function),
				stmts:     fn.body.Stmts,
				expr:      fn.body.Expr,
				index:     0,
			}
			for i, arg := range ex.Args {
				val, isReturn, err := eval(r, w, s, arg)
				if isReturn || err != nil {
					return val, isReturn, err
				}
				sc.idents[fn.params[i]] = ident{version: val.Version(), value: val}
			}
			val, _, err := interp(r, w, sc)
			return val, false, err
		} else if fn.lib != nil {
			args := []ident{}
			for _, arg := range ex.Args {
				val, isReturn, err := eval(r, w, s, arg)
				if isReturn || err != nil {
					return val, isReturn, err
				}
				args = append(args, ident{version: val.Version(), value: val})
			}
			vals := []*Value{}
			for i, arg := range args {
				if arg.version != arg.value.Version() {
					return nil, false, ex.Args[i].ExprFirstToken().Errorf("Expression refers to an unaccessible value")
				}
				vals = append(vals, arg.value)
			}
			return fn.lib.Call(fn.token, r, w, vals)
		} else {
			panic("Unknown function definition")
		}
	case *ExprBinary:
		left, isReturn, err := eval(r, w, s, ex.Left)
		if isReturn || err != nil {
			return left, isReturn, err
		}
		leftVersion := left.Version()
		right, isReturn, err := eval(r, w, s, ex.Right)
		if isReturn || err != nil {
			return right, isReturn, err
		}
		if leftVersion != left.Version() {
			return nil, false, ex.Left.ExprFirstToken().Errorf("Expression refers to an unaccessible value")
		}
		result := false
		switch ex.Op.Value {
		case "+":
			if left == right {
				return nil, false, ex.Op.Errorf("Expression pushes a value onto itself")
			}
			if ex.Block == nil {
				left.Push(right)
				return left, false, nil
			}
			if left.TryPush(right) {
				return left, false, nil
			}
			result = true
		case "<":
			result = left.Size() < right.Size()
		case ">":
			result = left.Size() > right.Size()
		case "=":
			result = left.Size() == right.Size()
		default:
			panic("Unknown binary operator")
		}
		if !result {
			return right, false, nil
		} else if ex.Block == nil {
			return left, false, nil
		}
		val, isReturn, err := interp(r, w, &scope{
			parent:    s,
			caller:    s.caller,
			idents:    make(map[string]ident),
			functions: make(map[string]function),
			stmts:     ex.Block.Stmts,
			expr:      ex.Block.Expr,
			index:     0,
		})
		return val, isReturn || ex.Block.Return, err
	case *ExprPop:
		val, isReturn, err := eval(r, w, s, ex.Expr)
		if isReturn || err != nil {
			return val, isReturn, err
		}
		popped := val.Pop()
		if popped == nil {
			return val, false, nil
		}
		sc := &scope{
			parent:    s,
			caller:    s.caller,
			idents:    make(map[string]ident),
			functions: make(map[string]function),
			stmts:     ex.Block.Block.Stmts,
			expr:      ex.Block.Block.Expr,
			index:     0,
		}
		sc.idents[ex.Block.Name.Value] = ident{version: popped.Version(), value: popped}
		return interp(r, w, sc)
	default:
		panic("Unknown expression")
	}
}
