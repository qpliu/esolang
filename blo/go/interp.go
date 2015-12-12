package main

func EvalFunc(funcDecl *Func, args []*Value) *Value {
	if funcDecl.Imported {
		return funcDecl.Runtime(funcDecl, args)
	}
	return interpStmt(NewScope(funcDecl, args), funcDecl.Body)
}

func interpStmt(scope Scope, stmt Stmt) *Value {
	for {
		switch st := stmt.(type) {
		case nil:
			return nil
		case *StmtBlock:
			if len(st.Stmts) > 0 {
				stmt = st.Stmts[0]
			} else {
				stmt = st.Next
			}
		case *StmtVar:
			scope[st.Var.Name] = nil
			if st.Expr != nil {
				scope[st.Var.Name] = EvalExpr(scope, st.Expr)
			}
			stmt = st.Next
		case *StmtIf:
			if EvalBitExpr(scope, st.Expr) {
				stmt = st.Stmts
			} else if st.ElseIf != nil {
				stmt = st.ElseIf
			} else if st.Else != nil {
				stmt = st.Else
			} else {
				stmt = st.Next
			}
		case *StmtFor:
			stmt = st.Stmts
		case StmtContinue:
			stmt = st.Next
		case *StmtBreak:
			stmt = st.Next
		case *StmtReturn:
			if st.Expr == nil {
				return nil
			} else {
				return EvalExpr(scope, st.Expr)
			}
		case *StmtSetClear:
			value := EvalExpr(scope, st.Expr.Expr)
			value.SetBitField(st.Expr.Name, st.Expr.Type(), st.Value)
			stmt = st.Next
		case *StmtAssign:
			switch lExpr := st.LValue.(type) {
			case *ExprVar:
				scope[lExpr.Name] = EvalExpr(scope, st.Expr)
			case *ExprField:
				if lExpr.IsBit() {
					EvalExpr(scope, lExpr.Expr).SetBitField(lExpr.Name, lExpr.Expr.Type(), EvalBitExpr(scope, st.Expr))
				} else {
					lvalue := EvalExpr(scope, st.LValue)
					rvalue := EvalExpr(scope, st.Expr)
					copy(lvalue.Bits, rvalue.Bits)
					copy(lvalue.Opaque, rvalue.Opaque)
				}
			default:
				lvalue := EvalExpr(scope, st.LValue)
				rvalue := EvalExpr(scope, st.Expr)
				copy(lvalue.Bits, rvalue.Bits)
				copy(lvalue.Opaque, rvalue.Opaque)
			}
			stmt = st.Next
		case *StmtExpr:
			EvalExpr(scope, st.Expr)
			stmt = st.Next
		default:
			panic("Unknown Stmt type")
		}
	}
}

func EvalExpr(scope Scope, expr Expr) *Value {
	switch ex := expr.(type) {
	case *ExprVar:
		if scope[ex.Name] == nil {
			scope[ex.Name] = NewValue(ex.Var.Type)
		}
		return scope[ex.Name]
	case *ExprField:
		return EvalExpr(scope, ex.Expr).Field(ex.Name, ex.Expr.Type())
	case *ExprFunc:
		args := make([]*Value, len(ex.Params))
		for i, param := range ex.Params {
			args[i] = EvalExpr(scope, param)
		}
		return EvalFunc(ex.Func, args)
	default:
		panic("Unknown Expr type")
	}
}

func EvalBitExpr(scope Scope, expr Expr) bool {
	fieldExpr := expr.(*ExprField)
	return EvalExpr(scope, fieldExpr.Expr).BitField(fieldExpr.Name, fieldExpr.Expr.Type())
}
