package main

import (
	"io"
)

type Ast struct {
	Types        map[string]*Type
	Funcs        map[string]*Func
	MaxBitIndex  int
	MaxOffset    int
	MaxLocalRefs int
}

func newAst() *Ast {
	return &Ast{Types: make(map[string]*Type), Funcs: make(map[string]*Func)}
}

type Type struct {
	Location Location
	Name     string
	Imported bool
	Fields   []*Var
}

func (t *Type) BitSize() int {
	if t == nil {
		return 1
	}
	bitSize := 0
	for _, field := range t.Fields {
		bitSize += field.Type.BitSize()
	}
	return bitSize
}

func (t *Type) OpaqueSize() int {
	if t == nil {
		return 0
	}
	opaqueSize := 0
	if t.Imported {
		opaqueSize++
	}
	for _, field := range t.Fields {
		opaqueSize += field.Type.OpaqueSize()
	}
	return opaqueSize
}

func (t *Type) Contains(typeDecl *Type) bool {
	for _, field := range t.Fields {
		if field.Type != nil && (field.Type == typeDecl || field.Type.Contains(typeDecl)) {
			return true
		}
	}
	return false
}

func (t *Type) BitOffset(fieldName string) int {
	offset := 0
	for _, field := range t.Fields {
		if fieldName == field.Name {
			return offset
		}
		offset += field.Type.BitSize()
	}
	panic("Unknown field " + fieldName)
}

type Func struct {
	Location    Location
	Name        string
	Imported    bool
	Params      []*Var
	TypeName    string
	Type        *Type
	Body        *StmtBlock
	Runtime     func(*Func, []*Value) *Value
	RuntimeLLVM func(*Ast, *Func, io.Writer) error
}

type Var struct {
	Name     string
	TypeName string
	Type     *Type
}

type Stmt interface {
	Location() Location
	Scope() map[string]*Var
	LLVMAnnotation() *LLVMStmtAnnotation
}

type StmtBlock struct {
	location Location
	Stmts    []Stmt
	Next     Stmt
	scope    map[string]*Var
	llvmAnn  LLVMStmtAnnotation
}

func (s *StmtBlock) Location() Location {
	return s.location
}

func (s *StmtBlock) Scope() map[string]*Var {
	return s.scope
}

func (s *StmtBlock) LLVMAnnotation() *LLVMStmtAnnotation {
	return &s.llvmAnn
}

type StmtVar struct {
	location Location
	Var      Var
	Expr     Expr
	Next     Stmt
	scope    map[string]*Var
	llvmAnn  LLVMStmtAnnotation
}

func (s *StmtVar) Location() Location {
	return s.location
}

func (s *StmtVar) Scope() map[string]*Var {
	return s.scope
}

func (s *StmtVar) LLVMAnnotation() *LLVMStmtAnnotation {
	return &s.llvmAnn
}

type StmtIf struct {
	location Location
	Expr     Expr
	Stmts    *StmtBlock
	ElseIf   *StmtIf
	Else     *StmtBlock
	Next     Stmt
	scope    map[string]*Var
	llvmAnn  LLVMStmtAnnotation
}

func (s *StmtIf) Location() Location {
	return s.location
}

func (s *StmtIf) Scope() map[string]*Var {
	return s.scope
}

func (s *StmtIf) LLVMAnnotation() *LLVMStmtAnnotation {
	return &s.llvmAnn
}

type StmtFor struct {
	location Location
	Label    string
	Stmts    *StmtBlock
	Next     Stmt
	scope    map[string]*Var
	llvmAnn  LLVMStmtAnnotation
}

func (s *StmtFor) Location() Location {
	return s.location
}

func (s *StmtFor) Scope() map[string]*Var {
	return s.scope
}

func (s *StmtFor) LLVMAnnotation() *LLVMStmtAnnotation {
	return &s.llvmAnn
}

type StmtContinue struct {
	Next *StmtFor
}

func (s StmtContinue) Location() Location {
	return s.Next.location
}

func (s StmtContinue) Scope() map[string]*Var {
	return s.Next.scope
}

func (s StmtContinue) LLVMAnnotation() *LLVMStmtAnnotation {
	return &s.Next.llvmAnn
}

type StmtBreak struct {
	location Location
	Label    string
	Next     Stmt
	scope    map[string]*Var
	llvmAnn  LLVMStmtAnnotation
}

func (s *StmtBreak) Location() Location {
	return s.location
}

func (s *StmtBreak) Scope() map[string]*Var {
	return s.scope
}

func (s *StmtBreak) LLVMAnnotation() *LLVMStmtAnnotation {
	return &s.llvmAnn
}

type StmtReturn struct {
	location Location
	Expr     Expr
	scope    map[string]*Var
	llvmAnn  LLVMStmtAnnotation
}

func (s *StmtReturn) Location() Location {
	return s.location
}

func (s *StmtReturn) Scope() map[string]*Var {
	return s.scope
}

func (s *StmtReturn) LLVMAnnotation() *LLVMStmtAnnotation {
	return &s.llvmAnn
}

type StmtSetClear struct {
	location Location
	Value    bool
	Expr     *ExprField
	Next     Stmt
	scope    map[string]*Var
	llvmAnn  LLVMStmtAnnotation
}

func (s *StmtSetClear) Location() Location {
	return s.location
}

func (s *StmtSetClear) Scope() map[string]*Var {
	return s.scope
}

func (s *StmtSetClear) LLVMAnnotation() *LLVMStmtAnnotation {
	return &s.llvmAnn
}

type StmtAssign struct {
	LValue, Expr Expr
	Next         Stmt
	scope        map[string]*Var
	llvmAnn      LLVMStmtAnnotation
}

func (s *StmtAssign) Location() Location {
	return s.LValue.Location()
}

func (s *StmtAssign) Scope() map[string]*Var {
	return s.scope
}

func (s *StmtAssign) LLVMAnnotation() *LLVMStmtAnnotation {
	return &s.llvmAnn
}

type StmtExpr struct {
	Expr    Expr
	Next    Stmt
	scope   map[string]*Var
	llvmAnn LLVMStmtAnnotation
}

func (s *StmtExpr) Location() Location {
	return s.Expr.Location()
}

func (s *StmtExpr) Scope() map[string]*Var {
	return s.scope
}

func (s *StmtExpr) LLVMAnnotation() *LLVMStmtAnnotation {
	return &s.llvmAnn
}

type Expr interface {
	Location() Location
	Type() *Type
	IsBit() bool
	LLVMAnnotation() *LLVMExprAnnotation
}

type ExprVar struct {
	location Location
	Name     string
	Var      *Var
	llvmAnn  LLVMExprAnnotation
}

func (e *ExprVar) Location() Location {
	return e.location
}

func (e *ExprVar) Type() *Type {
	if e.Var == nil {
		return nil
	}
	return e.Var.Type
}

func (e *ExprVar) IsBit() bool {
	return false
}

func (e *ExprVar) LLVMAnnotation() *LLVMExprAnnotation {
	return &e.llvmAnn
}

type ExprField struct {
	Name    string
	Expr    Expr
	llvmAnn LLVMExprAnnotation
}

func (e *ExprField) Location() Location {
	return e.Expr.Location()
}

func (e *ExprField) Type() *Type {
	t := e.Expr.Type()
	if t == nil {
		return nil
	}
	for _, field := range t.Fields {
		if field.Name == e.Name {
			return field.Type
		}
	}
	return nil
}

func (e *ExprField) IsBit() bool {
	return e.IsValid() && e.Type() == nil
}

func (e *ExprField) LLVMAnnotation() *LLVMExprAnnotation {
	return &e.llvmAnn
}

func (e *ExprField) IsValid() bool {
	t := e.Expr.Type()
	if t == nil {
		return false
	}
	for _, field := range t.Fields {
		if field.Name == e.Name {
			return true
		}
	}
	return false
}

type ExprFunc struct {
	location Location
	Name     string
	Params   []Expr
	Func     *Func
	llvmAnn  LLVMExprAnnotation
}

func (e *ExprFunc) Location() Location {
	return e.location
}

func (e *ExprFunc) Type() *Type {
	if e.Func == nil {
		return nil
	}
	return e.Func.Type
}

func (e *ExprFunc) IsBit() bool {
	return false
}

func (e *ExprFunc) LLVMAnnotation() *LLVMExprAnnotation {
	return &e.llvmAnn
}

func GoingOutOfScope(stmt, next Stmt) []*Var {
	var vars []*Var
	if next == nil {
		for _, v := range stmt.Scope() {
			vars = append(vars, v)
		}
	} else {
		nextScope := next.Scope()
		for name, v := range stmt.Scope() {
			if _, ok := nextScope[name]; !ok {
				vars = append(vars, v)
			}
		}
	}
	return vars
}

func WalkStmts(funcDecl *Func, f func(Stmt, bool) error) error {
	var walk func(Stmt, bool) error
	walk = func(stmt Stmt, inLoop bool) error {
		switch st := stmt.(type) {
		case nil:
		case *StmtBlock:
			if st != nil {
				if err := f(stmt, inLoop); err != nil {
					return err
				}
				for _, s := range st.Stmts {
					if err := walk(s, inLoop); err != nil {
						return err
					}
				}
			}
		case *StmtIf:
			if st != nil {
				if err := f(stmt, inLoop); err != nil {
					return err
				}
				if err := walk(st.Stmts, inLoop); err != nil {
					return err
				}
				if err := walk(st.ElseIf, inLoop); err != nil {
					return err
				}
				if err := walk(st.Else, inLoop); err != nil {
					return err
				}
			}
		case *StmtFor:
			if err := f(stmt, inLoop); err != nil {
				return err
			}
			if err := walk(st.Stmts, true); err != nil {
				return err
			}
		default:
			if err := f(stmt, inLoop); err != nil {
				return err
			}
		}
		return nil
	}
	return walk(funcDecl.Body, false)
}

func WalkExprs(funcDecl *Func, f func(Stmt, bool, Expr, bool) error) error {
	var walk func(Stmt, bool, Expr, bool) error
	walk = func(stmt Stmt, inLoop bool, expr Expr, inAssign bool) error {
		switch ex := expr.(type) {
		case *ExprField:
			if err := walk(stmt, inLoop, ex.Expr, inAssign); err != nil {
				return err
			}
			return f(stmt, inLoop, expr, inAssign)
		case *ExprFunc:
			for _, param := range ex.Params {
				if err := walk(stmt, inLoop, param, inAssign); err != nil {
					return err
				}
			}
			return f(stmt, inLoop, expr, inAssign)
		default:
			return f(stmt, inLoop, expr, inAssign)
		}
	}
	return WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		switch st := stmt.(type) {
		case *StmtVar:
			if st.Expr == nil {
				return nil
			}
			return walk(stmt, inLoop, st.Expr, true)
		case *StmtIf:
			return walk(stmt, inLoop, st.Expr, false)
		case *StmtReturn:
			if st.Expr == nil {
				return nil
			}
			return walk(stmt, inLoop, st.Expr, false)
		case *StmtSetClear:
			return walk(stmt, inLoop, st.Expr, false)
		case *StmtAssign:
			if err := walk(stmt, inLoop, st.LValue, false); err != nil {
				return err
			}
			_, ok := st.LValue.(*ExprVar)
			return walk(stmt, inLoop, st.Expr, ok)
		case *StmtExpr:
			return walk(stmt, inLoop, st.Expr, false)
		default:
			return nil
		}
	})
}
