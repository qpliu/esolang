package main

type loopScope struct {
	parent    *loopScope
	stmt      *Stmt
	stmtIndex int
	loopName  string

	vars, prevVars map[string]bool
	queues         map[string]Queue
}

func Interp(stmt *Stmt, ioQueue Queue) {
	if stmt.Type != StmtLoop || len(stmt.Idents) != 0 {
		panic("Interp:stmt.Type")
	}
	interp(&loopScope{
		parent: &loopScope{
			vars:     map[string]bool{"0": false},
			prevVars: map[string]bool{},
			queues:   map[string]Queue{"io": ioQueue},
		},
		stmt:      stmt,
		stmtIndex: 0,
		loopName:  "",

		vars:     map[string]bool{},
		prevVars: map[string]bool{},
		queues:   map[string]Queue{},
	})
}

func (scope *loopScope) exit() {
	for _, q := range scope.queues {
		q.Close()
	}
}

func (scope *loopScope) nextIteration() {
	scope.stmtIndex = 0
	for k, v := range scope.vars {
		scope.prevVars[k] = v
	}
	scope.vars = map[string]bool{}
	for _, q := range scope.queues {
		q.Close()
	}
	scope.queues = map[string]Queue{}
}

func (scope *loopScope) fork(stmt *Stmt) *loopScope {
	if stmt.Type != StmtFork {
		panic("fork:stmt.Type")
	}
	queue := stmt.Idents[0]
	if len(stmt.Idents) > 1 {
		stmt = stmt.ForkScope[stmt.Idents[1].T]
		if stmt == nil || stmt.Type != StmtFork || len(stmt.Idents) != 1 {
			panic("fork:queue reference")
		}
	}
	if _, ok := scope.queues[queue.T]; ok {
		panic(queue.Loc() + ": queue " + queue.T)
	}
	q1, q2 := NewQueue()
	scope.queues[queue.T] = q1

	freezeScope := scope
findFreezeScope:
	for freezeScope.stmt != nil {
		for _, s := range freezeScope.stmt.Stmts {
			if s == stmt {
				break findFreezeScope
			}
		}
		freezeScope = freezeScope.parent
	}

	vars := map[string]bool{}
	prevVars := map[string]bool{}
	for sc := freezeScope; sc != nil; sc = sc.parent {
		for k, v := range sc.vars {
			if _, ok := vars[k]; ok {
				panic(stmt.Token.Loc() + ":scope duplicates var " + k)
			}
			vars[k] = v
		}
		for k, v := range sc.prevVars {
			if _, ok := prevVars[k]; ok {
				panic(stmt.Token.Loc() + ":scope duplicates prevVar " + k)
			}
			prevVars[k] = v
		}
	}
	newScope := &loopScope{
		parent: &loopScope{
			vars:     vars,
			prevVars: prevVars,
			queues:   map[string]Queue{stmt.Idents[0].T: q2},
		},
		stmt:      stmt,
		stmtIndex: 0,

		vars:     map[string]bool{},
		prevVars: map[string]bool{},
		queues:   map[string]Queue{},
	}
	return newScope
}

func (scope *loopScope) getVar(ident Token) bool {
	for sc := scope; sc != nil; sc = sc.parent {
		if v, ok := sc.vars[ident.T]; ok {
			return v
		}
	}
	panic(ident.Loc() + ": var " + ident.T)
}

func (scope *loopScope) getPrevVar(ident Token) (bool, bool) {
	for sc := scope; sc != nil; sc = sc.parent {
		if v, ok := sc.prevVars[ident.T]; ok {
			return v, true
		}
	}
	return false, false
}

func (scope *loopScope) eval(expr *Expr) bool {
	switch expr.Type {
	case ExprNand:
		return !(scope.eval(expr.SubExpr1) && scope.eval(expr.SubExpr2))
	case ExprPrevVar:
		b, ok := scope.getPrevVar(expr.Token)
		if ok {
			return b
		}
		return scope.eval(expr.SubExpr1)
	case ExprVar:
		return scope.getVar(expr.Token)
	default:
		panic("eval:expr.Type")
	}
}

func interp(scope *loopScope) {
	for scope.stmt != nil {
		if scope.stmtIndex >= len(scope.stmt.Stmts) {
			scope.nextIteration()
			continue
		}
		stmt := scope.stmt.Stmts[scope.stmtIndex]
		switch stmt.Type {
		case StmtAssign:
			scope.vars[stmt.Idents[0].T] = scope.eval(stmt.Expr)
			scope.stmtIndex++
			continue
		case StmtBreak:
			if stmt.Expr != nil && !scope.eval(stmt.Expr) {
				scope.stmtIndex++
				continue
			}
			if len(stmt.Idents) > 0 {
				for stmt.Idents[0].T != scope.loopName {
					scope.exit()
					scope = scope.parent
				}
			}
			scope.exit()
			scope = scope.parent
			scope.stmtIndex++
			continue
		case StmtContinue:
			if stmt.Expr != nil && !scope.eval(stmt.Expr) {
				scope.stmtIndex++
				continue
			}
			if len(stmt.Idents) >= 0 {
				for stmt.Idents[0].T != scope.loopName {
					scope.exit()
					scope = scope.parent
				}
			}
			scope.nextIteration()
			continue
		case StmtFork:
			forkScope := scope.fork(stmt)
			go interp(forkScope)
			scope.stmtIndex++
			continue
		case StmtLoop:
			loopName := ""
			if len(stmt.Idents) > 0 {
				loopName = stmt.Idents[0].T
			}
			scope = &loopScope{
				parent:    scope,
				stmt:      stmt,
				stmtIndex: 0,
				loopName:  loopName,
				vars:      map[string]bool{},
				prevVars:  map[string]bool{},
				queues:    map[string]Queue{},
			}
			continue
		case StmtReceive:
			var queue Queue
			for sc := scope; queue == nil; sc = sc.parent {
				queue = sc.queues[stmt.Idents[0].T]
			}
			bit, open := queue.Receive()
			if open {
				scope.vars[stmt.Idents[1].T] = bit
				scope.stmtIndex++
				continue
			}
			if len(stmt.Idents) > 2 {
				for stmt.Idents[2].T != scope.loopName {
					scope.exit()
					scope = scope.parent
				}
			}
			scope.exit()
			scope = scope.parent
			scope.stmtIndex++
			continue
		case StmtSend:
			var queue Queue
			for sc := scope; queue == nil; sc = sc.parent {
				queue = sc.queues[stmt.Idents[0].T]
			}
			open := queue.Send(scope.eval(stmt.Expr))
			if open || len(stmt.Stmts) == 0 {
				scope.stmtIndex++
			} else {
				scope = &loopScope{
					parent:    scope,
					stmt:      stmt,
					stmtIndex: 0,
					loopName:  "",
					vars:      map[string]bool{},
					prevVars:  map[string]bool{},
					queues:    map[string]Queue{},
				}
			}
			continue
		}
		panic("interp")
	}
	scope.exit()
}
