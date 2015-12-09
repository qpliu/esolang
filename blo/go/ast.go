package main

type Ast struct {
	Types map[string]*Type
	Funcs map[string]*Func
}

func newAst() *Ast {
	ast := &Ast{}
	ast.Types = make(map[string]*Type)
	ast.Funcs = make(map[string]*Func)
	return ast
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

type Func struct {
	Location Location
	Name     string
	Imported bool
	Params   []*Var
	TypeName string
	Type     *Type
	Body     *StmtBlock
}

type Var struct {
	Name     string
	TypeName string
	Type     *Type
}

type Stmt interface {
	Location() Location
	CanFallThru() bool
}

type StmtBlock struct {
	location Location
	Stmts    []Stmt
}

func (s *StmtBlock) Location() Location {
	return s.location
}

func (s *StmtBlock) CanFallThru() bool {
	for _, stmt := range s.Stmts {
		if !stmt.CanFallThru() {
			return false
		}
	}
	return true
}

type StmtVar struct {
	location Location
	Name     string
	TypeName string
	Type     *Type
	Expr     Expr
}

func (s *StmtVar) Location() Location {
	return s.location
}

func (s *StmtVar) CanFallThru() bool {
	return true
}

type StmtIf struct {
	location Location
	Expr     Expr
	Stmts    *StmtBlock
	ElseIf   *StmtIf
	Else     *StmtBlock
}

func (s *StmtIf) Location() Location {
	return s.location
}

func (s *StmtIf) CanFallThru() bool {
	if s.Stmts.CanFallThru() {
		return true
	} else if s.ElseIf != nil {
		if s.Else != nil {
			panic("StmtIf should not have both else if and else")
		}
		return s.ElseIf.CanFallThru()
	} else if s.Else != nil {
		return s.Else.CanFallThru()
	} else {
		return true
	}
}

type StmtFor struct {
	location Location
	Label    string
	Stmts    *StmtBlock
}

func (s *StmtFor) Location() Location {
	return s.location
}

func (s *StmtFor) CanFallThru() bool {
	panic("Not yet implemented")
}

type StmtBreak struct {
	location Location
	Label    string
}

func (s *StmtBreak) Location() Location {
	return s.location
}

func (s *StmtBreak) CanFallThru() bool {
	return false
}

type StmtReturn struct {
	location Location
	Expr     Expr
}

func (s *StmtReturn) Location() Location {
	return s.location
}

func (s *StmtReturn) CanFallThru() bool {
	return false
}

type StmtSetClear struct {
	location Location
	Value    bool
	Expr     Expr
}

func (s *StmtSetClear) Location() Location {
	return s.location
}

func (s *StmtSetClear) CanFallThru() bool {
	return true
}

type StmtAssign struct {
	LValue, Expr Expr
}

func (s *StmtAssign) Location() Location {
	return s.LValue.Location()
}

func (s *StmtAssign) CanFallThru() bool {
	return true
}

type StmtExpr struct {
	Expr Expr
}

func (s *StmtExpr) Location() Location {
	return s.Expr.Location()
}

func (s *StmtExpr) CanFallThru() bool {
	return true
}

type Expr interface {
	Location() Location
	Type() *Type
}

type ExprVar struct {
	location Location
	Name     string
	Var      *Var
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

type ExprField struct {
	Name string
	Expr Expr
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

type ExprFunc struct {
	location Location
	Name     string
	Params   []Expr
	Func     *Func
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
