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
}

type StmtBlock struct {
	location Location
	Stmts    []Stmt
	Next     Stmt
}

func (s *StmtBlock) Location() Location {
	return s.location
}

type StmtVar struct {
	location Location
	Var      Var
	Expr     Expr
	Next     Stmt
}

func (s *StmtVar) Location() Location {
	return s.location
}

type StmtIf struct {
	location Location
	Expr     Expr
	Stmts    *StmtBlock
	ElseIf   *StmtIf
	Else     *StmtBlock
	Next     Stmt
}

func (s *StmtIf) Location() Location {
	return s.location
}

type StmtFor struct {
	location Location
	Label    string
	Stmts    *StmtBlock
	Next     Stmt
}

func (s *StmtFor) Location() Location {
	return s.location
}

type StmtBreak struct {
	location Location
	Label    string
	Next     Stmt
}

func (s *StmtBreak) Location() Location {
	return s.location
}

type StmtReturn struct {
	location Location
	Expr     Expr
}

func (s *StmtReturn) Location() Location {
	return s.location
}

type StmtSetClear struct {
	location Location
	Value    bool
	Expr     Expr
	Next     Stmt
}

func (s *StmtSetClear) Location() Location {
	return s.location
}

type StmtAssign struct {
	LValue, Expr Expr
	Next         Stmt
}

func (s *StmtAssign) Location() Location {
	return s.LValue.Location()
}

type StmtExpr struct {
	Expr Expr
	Next Stmt
}

func (s *StmtExpr) Location() Location {
	return s.Expr.Location()
}

type Expr interface {
	Location() Location
	Type() *Type
	IsBit() bool
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

func (e *ExprVar) IsBit() bool {
	return false
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

func (e *ExprField) IsBit() bool {
	return e.IsValid() && e.Type() == nil
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
