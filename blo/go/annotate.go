package main

import (
	"errors"
)

func (ast *Ast) Annotate() error {
	if err := annotateTypeDecls(ast); err != nil {
		return err
	}
	if err := annotateFuncDecls(ast); err != nil {
		return err
	}
	if err := annotateTypesInFuncBodies(ast); err != nil {
		return err
	}
	if err := annotateStatementFlows(ast); err != nil {
		return err
	}
	annotateStatementScopes(ast)
	annotateMaxBitIndex(ast)
	annotateMaxOffset(ast)
	annotateMaxLocalRefs(ast)
	return nil
}

func annotateTypeDecls(ast *Ast) error {
	for typeName, typeDecl := range ast.Types {
		for _, field := range typeDecl.Fields {
			if field.TypeName == "" {
			} else if fieldType, ok := ast.Types[field.TypeName]; !ok {
				return errors.New(typeDecl.Location.String() + ": Unknown type '" + field.TypeName + "' for field '" + field.Name + "' in type `" + typeName + "'")
			} else {
				field.Type = fieldType
			}
		}
	}
	typeSet := make(map[string]bool)
	for typeName, typeDecl := range ast.Types {
		typeSet[typeName] = true
		if err := checkRecursiveTypes(typeDecl, typeDecl, typeSet); err != nil {
			return err
		}
		delete(typeSet, typeName)
	}
	return nil
}

func checkRecursiveTypes(baseTypeDecl, typeDecl *Type, typeSet map[string]bool) error {
	for _, field := range typeDecl.Fields {
		if field.TypeName == "" {
		} else if _, ok := typeSet[field.TypeName]; ok {
			return errors.New(baseTypeDecl.Location.String() + ": Type contains recursive field of type '" + field.TypeName + "'")
		} else {
			typeSet[field.TypeName] = true
			if err := checkRecursiveTypes(baseTypeDecl, field.Type, typeSet); err != nil {
				return err
			}
			delete(typeSet, field.TypeName)
		}
	}
	return nil
}

func annotateFuncDecls(ast *Ast) error {
	for _, funcDecl := range ast.Funcs {
		if funcDecl.TypeName == "" {
		} else if typeDecl, ok := ast.Types[funcDecl.TypeName]; ok {
			funcDecl.Type = typeDecl
		} else {
			return errors.New(funcDecl.Location.String() + ": Function '" + funcDecl.Name + "' has unknown return type '" + funcDecl.TypeName + "'")
		}
		for _, param := range funcDecl.Params {
			if typeDecl, ok := ast.Types[param.TypeName]; ok {
				param.Type = typeDecl
			} else {
				return errors.New(funcDecl.Location.String() + ": Parameter '" + param.Name + "' of Function '" + funcDecl.Name + "' has unknown type '" + param.TypeName + "'")
			}
		}
	}
	return nil
}

type lexicalScope struct {
	vars   []*Var
	parent *lexicalScope
}

func (s *lexicalScope) lookup(name string) *Var {
	for _, v := range s.vars {
		if v.Name == name {
			return v
		}
	}
	if s.parent != nil {
		return s.parent.lookup(name)
	}
	return nil
}

func annotateTypesInFuncBodies(ast *Ast) error {
	for _, funcDecl := range ast.Funcs {
		if err := annotateTypesInStmt(ast, &lexicalScope{vars: funcDecl.Params}, funcDecl.Type, funcDecl.Body); err != nil {
			return err
		}
	}
	return nil
}

func annotateTypesInStmt(ast *Ast, scope *lexicalScope, returnType *Type, stmt Stmt) error {
	switch st := stmt.(type) {
	case nil:
		return nil
	case *StmtBlock:
		if st == nil {
			return nil
		}
		sc := &lexicalScope{parent: scope}
		for _, s := range st.Stmts {
			if err := annotateTypesInStmt(ast, sc, returnType, s); err != nil {
				return err
			}
		}
	case *StmtVar:
		if typeDecl, ok := ast.Types[st.Var.TypeName]; ok {
			st.Var.Type = typeDecl
		} else {
			return errors.New(st.Location().String() + ": Unknown type '" + st.Var.TypeName + "'")
		}
		if st.Expr != nil {
			if err := annotateTypesInExpr(ast, scope, st.Expr); err != nil {
				return err
			}
			if st.Expr.Type() != st.Var.Type {
				return errors.New(st.Location().String() + ": Initializer type mismatch")
			}
		}
		if scope.lookup(st.Var.Name) != nil {
			return errors.New(st.Location().String() + ": Duplicate variable name '" + st.Var.Name + "'")
		}
		scope.vars = append(scope.vars, &st.Var)
	case *StmtIf:
		if st == nil {
			return nil
		}
		if err := annotateTypesInExpr(ast, scope, st.Expr); err != nil {
			return err
		}
		if !st.Expr.IsBit() {
			return errors.New(st.Expr.Location().String() + ": the type of the condition expression in the if statement is not a bit field")
		}
		if err := annotateTypesInStmt(ast, scope, returnType, st.Stmts); err != nil {
			return err
		}
		if err := annotateTypesInStmt(ast, scope, returnType, st.ElseIf); err != nil {
			return err
		}
		if err := annotateTypesInStmt(ast, scope, returnType, st.Else); err != nil {
			return err
		}
	case *StmtFor:
		if err := annotateTypesInStmt(ast, scope, returnType, st.Stmts); err != nil {
			return err
		}
	case *StmtBreak:
		return nil
	case *StmtReturn:
		if err := annotateTypesInExpr(ast, scope, st.Expr); err != nil {
			return err
		}
		if returnType == nil {
			if st.Expr != nil {
				return errors.New(st.Location().String() + ": return from function with no return type cannot have expression")
			}
		} else if st.Expr == nil || returnType != st.Expr.Type() {
			return errors.New(st.Location().String() + ": return expression does not match the functions return type")
		}
	case *StmtSetClear:
		if err := annotateTypesInExpr(ast, scope, st.Expr); err != nil {
			return err
		}
		if !st.Expr.IsBit() {
			return errors.New(st.Location().String() + ": set/clear expression is not a bit field")
		}
	case *StmtAssign:
		if err := annotateTypesInExpr(ast, scope, st.LValue); err != nil {
			return err
		}
		if err := annotateTypesInExpr(ast, scope, st.Expr); err != nil {
			return err
		}
		if st.LValue.Type() == nil {
			if !st.LValue.IsBit() {
				return errors.New(st.Location().String() + ": invalid lvalue for assignment")
			} else if !st.Expr.IsBit() {
				return errors.New(st.Location().String() + ": type mismatch in assignment statement")
			}
		} else if st.LValue.Type() != st.Expr.Type() {
			return errors.New(st.Location().String() + ": type mismatch in assignment statement")
		}
	case *StmtExpr:
		if err := annotateTypesInExpr(ast, scope, st.Expr); err != nil {
			return err
		}
	}
	return nil
}

func annotateTypesInExpr(ast *Ast, scope *lexicalScope, expr Expr) error {
	switch ex := expr.(type) {
	case nil:
		return nil
	case *ExprVar:
		if varDecl := scope.lookup(ex.Name); varDecl == nil {
			return errors.New(ex.Location().String() + ": Undeclared variable '" + ex.Name + "'")
		} else {
			ex.Var = varDecl
		}
	case *ExprField:
		if err := annotateTypesInExpr(ast, scope, ex.Expr); err != nil {
			return err
		}
		if !ex.IsValid() {
			return errors.New(ex.Location().String() + ": Invalid field name '" + ex.Name + "'")
		}
	case *ExprFunc:
		for _, param := range ex.Params {
			if err := annotateTypesInExpr(ast, scope, param); err != nil {
				return err
			}
		}
		if funcDecl, ok := ast.Funcs[ex.Name]; ok {
			ex.Func = funcDecl
		} else {
			return errors.New(ex.Location().String() + ": Unknown function '" + ex.Name + "'")
		}
		if len(ex.Params) != len(ex.Func.Params) {
			return errors.New(ex.Location().String() + ": Wrong number of arguments to function '" + ex.Name + "'")
		}
		for i, param := range ex.Func.Params {
			if ex.Params[i].Type() != param.Type {
				return errors.New(ex.Location().String() + ": Type mismatch for an argument of function '" + ex.Name + "'")
			}
		}
	}
	return nil
}

type flowScope struct {
	label  string
	next   Stmt
	parent *flowScope
}

func (f *flowScope) lookup(label string) *flowScope {
	if f == nil {
		return nil
	}
	if label == "" || label == f.label {
		return f
	}
	return f.parent.lookup(label)
}

func annotateStatementFlows(ast *Ast) error {
	for _, funcDecl := range ast.Funcs {
		if funcDecl.Imported {
			continue
		}
		if err := annotateStmtFlow(nil, funcDecl.Body, nil); err != nil {
			return err
		}
		if funcDecl.Type != nil && canFallThru(funcDecl.Body) {
			return errors.New(funcDecl.Location.String() + ": Missing required return statement")
		}
	}
	return nil
}

func annotateStmtFlow(scope *flowScope, stmt, next Stmt) error {
	switch st := stmt.(type) {
	case nil:
	case *StmtBlock:
		if st == nil {
			return nil
		}
		if len(st.Stmts) == 0 {
			st.Next = next
			return nil
		}
		for i := 1; i < len(st.Stmts); i++ {
			if err := annotateStmtFlow(scope, st.Stmts[i-1], st.Stmts[i]); err != nil {
				return err
			} else if !canReach(st.Stmts[i-1], st.Stmts[i]) {
				return errors.New(st.Stmts[i].Location().String() + ": Unreachable")
			}
		}
		if err := annotateStmtFlow(scope, st.Stmts[len(st.Stmts)-1], next); err != nil {
			return err
		}
	case *StmtVar:
		st.Next = next
	case *StmtIf:
		if st == nil {
			return nil
		}
		if err := annotateStmtFlow(scope, st.Stmts, next); err != nil {
			return err
		}
		if err := annotateStmtFlow(scope, st.ElseIf, next); err != nil {
			return err
		}
		if err := annotateStmtFlow(scope, st.Else, next); err != nil {
			return err
		}
		if st.ElseIf == nil && st.Else == nil {
			st.Next = next
		}
	case *StmtFor:
		if err := annotateStmtFlow(&flowScope{label: st.Label, next: next, parent: scope}, st.Stmts, StmtContinue{st}); err != nil {
			return err
		}
	case *StmtBreak:
		flowScope := scope.lookup(st.Label)
		if flowScope == nil {
			return errors.New(st.Location().String() + ": Invalid break statement")
		}
		st.Next = flowScope.next
	case *StmtReturn:
	case *StmtSetClear:
		st.Next = next
	case *StmtAssign:
		st.Next = next
	case *StmtExpr:
		st.Next = next
	default:
		panic("Unknown statement type")
	}
	return nil
}

func canReach(fromStmt, toStmt Stmt) bool {
	if fromStmt == toStmt {
		return true
	}
	switch stmt := fromStmt.(type) {
	case nil:
		return false
	case *StmtBlock:
		if len(stmt.Stmts) == 0 {
			return canReach(stmt.Next, toStmt)
		} else {
			return canReach(stmt.Stmts[0], toStmt)
		}
	case *StmtVar:
		return canReach(stmt.Next, toStmt)
	case *StmtIf:
		return canReach(stmt.Stmts, toStmt) || (stmt.ElseIf != nil && canReach(stmt.ElseIf, toStmt)) || (stmt.Else != nil && canReach(stmt.Else, toStmt)) || (stmt.Next != nil && canReach(stmt.Next, toStmt))
	case *StmtFor:
		return canReach(stmt.Stmts, toStmt)
	case StmtContinue:
		return false
	case *StmtBreak:
		return canReach(stmt.Next, toStmt)
	case *StmtReturn:
		return false
	case *StmtSetClear:
		return canReach(stmt.Next, toStmt)
	case *StmtAssign:
		return canReach(stmt.Next, toStmt)
	case *StmtExpr:
		return canReach(stmt.Next, toStmt)
	default:
		panic("Unknown statement type")
	}
}

func canFallThru(fromStmt Stmt) bool {
	switch stmt := fromStmt.(type) {
	case *StmtBlock:
		return len(stmt.Stmts) == 0 || canFallThru(stmt.Stmts[len(stmt.Stmts)-1])
	case *StmtIf:
		return (stmt.ElseIf == nil && stmt.Else == nil) || canFallThru(stmt.Stmts) || (stmt.ElseIf != nil && canFallThru(stmt.ElseIf)) || (stmt.Else != nil && canFallThru(stmt.Else))
	case *StmtFor:
		return canBreak(stmt.Label, true, stmt.Stmts)
	case StmtContinue:
		return false
	case *StmtBreak, *StmtReturn:
		return false
	default:
		return true
	}
}

func canBreak(label string, unlabeled bool, fromStmt Stmt) bool {
	switch stmt := fromStmt.(type) {
	case *StmtBlock:
		for _, st := range stmt.Stmts {
			if canBreak(label, unlabeled, st) {
				return true
			}
		}
		return false
	case *StmtIf:
		return canBreak(label, unlabeled, stmt.Stmts) || (stmt.ElseIf != nil && canBreak(label, unlabeled, stmt.ElseIf)) || (stmt.Else != nil && canBreak(label, unlabeled, stmt.Else))
	case *StmtFor:
		if label == "" {
			return false
		}
		return canBreak(label, false, stmt.Stmts)
	case *StmtBreak:
		if label != "" && stmt.Label == label {
			return true
		}
		if unlabeled && stmt.Label == "" {
			return true
		}
		return false
	default:
		return false
	}
}

type stmtScope struct {
	vars   map[string]*Var
	parent *stmtScope
}

func newStmtScope(funcDecl *Func) *stmtScope {
	vars := make(map[string]*Var)
	for _, p := range funcDecl.Params {
		vars[p.Name] = p
	}
	return &stmtScope{vars: vars}
}

func (s *stmtScope) addVar(v *Var) {
	s.vars[v.Name] = v
}

func (s *stmtScope) newScope() *stmtScope {
	return &stmtScope{vars: make(map[string]*Var), parent: s}
}

func (s *stmtScope) makeScope() map[string]*Var {
	vars := make(map[string]*Var)
	for scope := s; scope != nil; scope = scope.parent {
		for n, v := range scope.vars {
			vars[n] = v
		}
	}
	return vars
}

func annotateStatementScopes(ast *Ast) {
	for _, funcDecl := range ast.Funcs {
		if funcDecl.Imported {
			continue
		}
		annotateStmtScopes(newStmtScope(funcDecl), funcDecl.Body)
	}
}

func annotateStmtScopes(scope *stmtScope, stmt Stmt) {
	switch st := stmt.(type) {
	case nil:
	case *StmtBlock:
		if st == nil {
			return
		}
		st.scope = scope.makeScope()
		stmtScope := scope.newScope()
		for _, s := range st.Stmts {
			annotateStmtScopes(stmtScope, s)
		}
	case *StmtVar:
		scope.addVar(&st.Var)
		st.scope = scope.makeScope()
	case *StmtIf:
		if st == nil {
			return
		}
		st.scope = scope.makeScope()
		annotateStmtScopes(scope, st.Stmts)
		annotateStmtScopes(scope, st.ElseIf)
		annotateStmtScopes(scope, st.Else)
	case *StmtFor:
		st.scope = scope.makeScope()
		annotateStmtScopes(scope, st.Stmts)
	case *StmtBreak:
		st.scope = scope.makeScope()
	case *StmtReturn:
		st.scope = scope.makeScope()
	case *StmtSetClear:
		st.scope = scope.makeScope()
	case *StmtAssign:
		st.scope = scope.makeScope()
	case *StmtExpr:
		st.scope = scope.makeScope()
	default:
		panic("Unknown statement type")
	}
}

func annotateMaxBitIndex(ast *Ast) {
	for _, typeDecl := range ast.Types {
		if typeDecl.BitSize()-1 > ast.MaxBitIndex {
			ast.MaxBitIndex = typeDecl.BitSize() - 1
		}
	}
}

func getMaxOffset(typeDecl *Type) int {
	offset := 0
	maxOffset := 0
	for _, field := range typeDecl.Fields {
		if field.Type != nil {
			maxOffset = offset + getMaxOffset(field.Type)
		}
		offset += field.Type.BitSize()
	}
	return maxOffset
}

func annotateMaxOffset(ast *Ast) {
	for _, typeDecl := range ast.Types {
		max := getMaxOffset(typeDecl)
		if max > ast.MaxOffset {
			ast.MaxOffset = max
		}
	}
}

func getMaxLocalRefs(funcDecl *Func) int {
	maxRefs := 0
	WalkStmts(funcDecl.Body, func(stmt Stmt, inLoop bool) {
		if st, ok := stmt.(*StmtVar); ok && inLoop {
			refs := 0
			for _, v := range st.Scope() {
				if st.Var.Type == v.Type || st.Var.Type.Contains(v.Type) {
					refs++
				}
			}
			if refs > maxRefs {
				maxRefs = refs
			}
		}
	})
	return maxRefs
}

func annotateMaxLocalRefs(ast *Ast) {
	for _, funcDecl := range ast.Funcs {
		max := getMaxLocalRefs(funcDecl)
		if max > ast.MaxLocalRefs {
			ast.MaxLocalRefs = max
		}
	}
}
