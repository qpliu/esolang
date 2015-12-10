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

func annotateStatementFlows(ast *Ast) error {
	//... panic("Not yet implemented") //...
	return nil
}
