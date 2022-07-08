package main

type scope struct {
	parent       *scope
	stmt         *Stmt
	index        int
	declarations map[string]bool
	vars         map[string]bool
	oldVars      map[string]bool
}

func Interpret(stmt *Stmt, vars map[string]bool, queues map[string][2]chan bool) {
	if stmt.Type != StmtLoop {
		panic("Interpret")
	}
	declarations := map[string]bool{}
	for k := range vars {
		declarations[k] = true
	}
	sc := &scope{
		parent: &scope{
			parent:       nil,
			stmt:         nil,
			index:        0,
			declarations: declarations,
			vars:         vars,
			oldVars:      vars,
		},
		stmt:         stmt,
		index:        0,
		declarations: stmt.Declarations,
		vars:         map[string]bool{},
		oldVars:      nil,
	}
	for {
		if sc == nil || sc.stmt == nil {
			break
		}
		if sc.index >= len(sc.stmt.Stmts) {
			sc.index = 0
			if sc.index >= len(sc.stmt.Stmts) {
				continue // empty loop is infinite loop
			}
			sc.oldVars = sc.vars
			sc.vars = map[string]bool{}
		}
		stmt = sc.stmt.Stmts[sc.index]
		switch stmt.Type {
		case StmtAssign:
			if len(stmt.Stmts) > 0 || len(stmt.Identifiers) != 1 || stmt.Expr == nil {
				panic("Interpret:StmtAssign")
			}
			if _, ok := sc.vars[stmt.Identifiers[0]]; ok {
				panic("Interpret:StmtAssign")
			}
			sc.vars[stmt.Identifiers[0]] = eval(stmt.Expr, sc)
			sc.index++
		case StmtBreak:
			if len(stmt.Stmts) > 0 || len(stmt.Identifiers) > 1 || stmt.Expr == nil {
				panic("Interpret:StmtBreak")
			}
			if eval(stmt.Expr, sc) {
				if len(stmt.Identifiers) == 0 {
					sc = sc.parent
				} else {
					for {
						if len(sc.stmt.Identifiers) > 0 && sc.stmt.Identifiers[0] == stmt.Identifiers[0] {
							sc = sc.parent
							break
						}
						sc = sc.parent
					}
				}
			}
			sc.index++
		case StmtContinue:
			if len(stmt.Stmts) > 0 || len(stmt.Identifiers) > 1 || stmt.Expr == nil {
				panic("Interpret:StmtContinue")
			}
			if eval(stmt.Expr, sc) {
				if len(stmt.Identifiers) == 0 {
					sc.index = len(sc.stmt.Stmts)
				} else {
					for {
						if len(sc.stmt.Identifiers) > 0 && sc.stmt.Identifiers[0] == stmt.Identifiers[0] {
							sc.index = len(sc.stmt.Stmts)
							break
						}
						sc = sc.parent
					}
				}
			}
			sc.index++
		case StmtLoop:
			if len(stmt.Identifiers) > 1 || stmt.Expr != nil {
				panic("Interpret:StmtLoop")
			}
			sc = &scope{
				parent:       sc,
				stmt:         stmt,
				index:        0,
				declarations: stmt.Declarations,
				vars:         map[string]bool{},
				oldVars:      nil,
			}
		case StmtReceive:
			if len(stmt.Stmts) > 0 || len(stmt.Identifiers) != 2 || stmt.Expr != nil {
				panic("Interpret:StmtReceive")
			}
			if _, ok := sc.vars[stmt.Identifiers[1]]; ok {
				panic("Interpret:StmtReceive")
			}
			ch, ok := queues[stmt.Identifiers[0]]
			if !ok {
				panic("Interpret:StmtReceive:" + stmt.Identifiers[0])
			}
			sc.vars[stmt.Identifiers[1]] = <-ch[0]
			sc.index++
		case StmtSend:
			if len(stmt.Stmts) > 0 || len(stmt.Identifiers) != 1 || stmt.Expr == nil {
				panic("Interpret:StmtSend")
			}
			ch, ok := queues[stmt.Identifiers[0]]
			if !ok {
				panic("Interpret:StmtSend:" + stmt.Identifiers[0])
			}
			ch[1] <- eval(stmt.Expr, sc)
			sc.index++
		}
	}
}

func eval(expr *Expr, sc *scope) bool {
	switch expr.Type {
	case ExprVar:
		if expr.Identifier == "" || len(expr.Exprs) > 0 {
			panic("eval:ExprVar")
		}
		for {
			if sc.declarations[expr.Identifier] {
				if b, ok := sc.vars[expr.Identifier]; ok {
					return b
				} else {
					panic("eval:ExprVar:" + expr.Identifier)
				}
			} else {
				sc = sc.parent
			}
		}
	case ExprOldVar:
		if expr.Identifier == "" || len(expr.Exprs) != 1 {
			panic("eval:ExprOldVar")
		}
		originalScope := sc
		for {
			if sc.declarations[expr.Identifier] {
				if sc.oldVars == nil {
					return eval(expr.Exprs[0], originalScope)
				}
				if b, ok := sc.oldVars[expr.Identifier]; ok {
					return b
				} else {
					panic("eval:ExprOldVar:" + expr.Identifier)
				}
			} else {
				sc = sc.parent
			}
		}
	case ExprNand:
		if expr.Identifier != "" || len(expr.Exprs) != 2 {
			panic("eval:ExprNand")
		}
		return !(eval(expr.Exprs[0], sc) && eval(expr.Exprs[1], sc))
	default:
		panic("eval")
	}
}
