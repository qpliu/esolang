package main

type Stmt interface {
	StmtFirstToken() Token
}

type StmtAssignment struct {
	Name Token
	Expr Expr
}

func (s *StmtAssignment) StmtFirstToken() Token {
	return s.Name
}

type StmtDefineFunc struct {
	Name   Token
	Params []Token
	Body   StmtBlock
}

func (s *StmtDefineFunc) StmtFirstToken() Token {
	return s.Name
}

type StmtDefineLibFunc struct {
	Name   Token
	Params []Token
	Lib    Token
}

func (s *StmtDefineLibFunc) StmtFirstToken() Token {
	return s.Name
}

type StmtExpr struct {
	Expr Expr
}

func (s *StmtExpr) StmtFirstToken() Token {
	return s.Expr.ExprFirstToken()
}

type StmtBlock struct {
	Stmts  []Stmt
	Expr   Expr
	Return bool
}

func (s *StmtBlock) StmtFirstToken() Token {
	if len(s.Stmts) > 0 {
		return s.Stmts[0].StmtFirstToken()
	}
	return s.Expr.ExprFirstToken()
}

type Expr interface {
	ExprFirstToken() Token
}

type Expr0 struct {
	Token Token
}

func (e *Expr0) ExprFirstToken() Token {
	return e.Token
}

type ExprIdentifier struct {
	Name Token
}

func (e *ExprIdentifier) ExprFirstToken() Token {
	return e.Name
}

type ExprCallFunction struct {
	Name Token
	Args []Expr
}

func (e *ExprCallFunction) ExprFirstToken() Token {
	return e.Name
}

type ExprBinary struct {
	Left, Right Expr
	Op          Token
	Block       *StmtBlock
}

func (e *ExprBinary) ExprFirstToken() Token {
	return e.Left.ExprFirstToken()
}

type ExprPop struct {
	Expr  Expr
	Block *struct {
		Name  Token
		Block StmtBlock
	}
}

func (e *ExprPop) ExprFirstToken() Token {
	return e.Expr.ExprFirstToken()
}
