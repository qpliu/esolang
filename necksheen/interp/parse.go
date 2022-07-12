package main

import (
	"fmt"
	"strings"
)

type StmtType int

const (
	StmtAssign StmtType = iota
	StmtBreak
	StmtContinue
	StmtFork
	StmtLoop
	StmtReceive
	StmtSend
)

type ExprType int

const (
	ExprNand ExprType = iota
	ExprPrevVar
	ExprVar
)

type Stmt struct {
	Type  StmtType
	Token Token

	Stmts  []*Stmt
	Idents []Token
	Expr   *Expr

	varScope   map[string]Token
	prescope   map[string]Token
	loopScope  map[string]Token
	queueScope map[string]Token
	forkScope  map[string]*Stmt
}

type Expr struct {
	Type  ExprType
	Token Token

	SubExpr1, SubExpr2 *Expr
}

func Parse(tokenizer *Tokenizer) (*Stmt, error) {
	stmts := []*Stmt{}
	for {
		if _, ok := tokenizer.Token(); !ok {
			break
		}
		if stmt, err := parseStmt(tokenizer); err != nil {
			return nil, err
		} else {
			stmts = append(stmts, stmt)
		}
	}
	stmt := &Stmt{
		Type:  StmtLoop,
		Stmts: stmts,
	}
	stmt.calculateVarScopes(map[string]Token{"0": Token{}})
	stmt.calculatePrescopes(map[string]Token{})
	stmt.calculateLoopScopes(map[string]Token{})
	stmt.calculateForkScopes(map[string]*Stmt{})
	stmt.calculateQueueScopes(map[string]Token{"io": Token{}})
	return stmt, stmt.check()
}

func parseStmt(tokenizer *Tokenizer) (*Stmt, error) {
	tok, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	switch tok.T {
	case "{":
		return parseStmtLoop(&Stmt{
			Type:  StmtLoop,
			Token: tok,
		}, tokenizer)
	case "break":
		return parseStmtExpr(&Stmt{
			Type:  StmtBreak,
			Token: tok,
		}, false, tokenizer)
	case "continue":
		return parseStmtExpr(&Stmt{
			Type:  StmtContinue,
			Token: tok,
		}, false, tokenizer)
	}
	if !tok.IsIdent() {
		return nil, fmt.Errorf("%s: unexpected token: %s", tok.Loc(), tok.T)
	}
	ident := tok
	tokenizer.Next()
	tok, ok = tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	switch tok.T {
	case "=":
		return parseStmtExpr(&Stmt{
			Type:   StmtAssign,
			Token:  ident,
			Idents: []Token{ident},
		}, false, tokenizer)
	case "<":
		return parseStmtExpr(&Stmt{
			Type:   StmtSend,
			Token:  ident,
			Idents: []Token{ident},
		}, true, tokenizer)
	case ">":
		return parseStmtReceive(ident, tokenizer)
	case "{":
		return parseStmtLoop(&Stmt{
			Type:   StmtLoop,
			Token:  ident,
			Idents: []Token{ident},
		}, tokenizer)
	case "+":
		return parseStmtFork(ident, tokenizer)
	case "break":
		return parseStmtOptionalExpr(&Stmt{
			Type:   StmtBreak,
			Token:  ident,
			Idents: []Token{ident},
		}, tokenizer)
	case "continue":
		return parseStmtOptionalExpr(&Stmt{
			Type:   StmtContinue,
			Token:  ident,
			Idents: []Token{ident},
		}, tokenizer)
	default:
		return nil, fmt.Errorf("%s: unexpected token: %s", tok.Loc(), tok.T)
	}
}

func parseStmtLoop(stmt *Stmt, tokenizer *Tokenizer) (*Stmt, error) {
	tokenizer.Next()
	for {
		tok, ok := tokenizer.Token()
		if !ok {
			return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
		}
		if tok.T == "}" {
			tokenizer.Next()
			return stmt, nil
		}
		if nested, err := parseStmt(tokenizer); err != nil {
			return nil, err
		} else {
			stmt.Stmts = append(stmt.Stmts, nested)
		}
	}
}

func parseStmtExpr(stmt *Stmt, loopMayFollow bool, tokenizer *Tokenizer) (*Stmt, error) {
	expr, err := parseExpr(tokenizer)
	if err != nil {
		return nil, err
	}
	stmt.Expr = expr
	tok, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	if tok.T == "." {
		tokenizer.Next()
		return stmt, nil
	}
	if tok.T != "{" || !loopMayFollow {
		return nil, fmt.Errorf("%s: unexpected token: %s", tok.Loc(), tok.T)
	}
	return parseStmtLoop(stmt, tokenizer)
}

func parseStmtOptionalExpr(stmt *Stmt, tokenizer *Tokenizer) (*Stmt, error) {
	tokenizer.Next()
	tok, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	if tok.T == "." {
		tokenizer.Next()
		return stmt, nil
	}
	tokenizer.Push(tok)
	return parseStmtExpr(stmt, false, tokenizer)
}

func parseStmtReceive(ident Token, tokenizer *Tokenizer) (*Stmt, error) {
	tokenizer.Next()
	tok, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	if !tok.IsIdent() {
		return nil, fmt.Errorf("%s: unexpected token: %s", tok.Loc(), tok.T)
	}
	stmt := &Stmt{
		Type:   StmtReceive,
		Token:  ident,
		Idents: []Token{ident, tok},
	}

	tok, ok = tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	if tok.IsIdent() {
		stmt.Idents = append(stmt.Idents, tok)
		tok, ok = tokenizer.Token()
		if !ok {
			return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
		}
	}
	if tok.T != "." {
		return nil, fmt.Errorf("%s: unexpected token: %s", tok.Loc(), tok.T)
	}
	tokenizer.Next()
	return stmt, nil
}

func parseStmtFork(ident Token, tokenizer *Tokenizer) (*Stmt, error) {
	tokenizer.Next()
	tok, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	if tok.T == "{" {
		return parseStmtLoop(&Stmt{
			Type:   StmtFork,
			Token:  ident,
			Idents: []Token{ident},
		}, tokenizer)
	}
	if !tok.IsIdent() {
		return nil, fmt.Errorf("%s: unexpected token: %s", tok.Loc(), tok.T)
	}
	idents := []Token{ident, tok}
	tok, ok = tokenizer.Token()
	if tok.T != "." {
		return nil, fmt.Errorf("%s: unexpected token: %s", tok.Loc(), tok.T)
	}
	return &Stmt{
		Type:   StmtFork,
		Token:  ident,
		Idents: idents,
	}, nil
}

func (stmt *Stmt) calculateVarScopes(varScope map[string]Token) {
	stmt.varScope = varScope
	for _, nestedStmt := range stmt.Stmts {
		nestedStmt.calculateVarScopes(varScope)
		var ident Token
		switch nestedStmt.Type {
		case StmtAssign:
			ident = nestedStmt.Idents[0]
		case StmtReceive:
			ident = nestedStmt.Idents[1]
		default:
			continue
		}
		newVarScope := map[string]Token{}
		for k, v := range varScope {
			newVarScope[k] = v
		}
		newVarScope[ident.T] = ident
		varScope = newVarScope
	}
}

func (stmt *Stmt) calculatePrescopes(prescope map[string]Token) {
	stmt.prescope = prescope
	for i := len(stmt.Stmts)-1; i >= 0; i-- {
		nestedStmt := stmt.Stmts[i]
		var ident Token
		switch nestedStmt.Type {
		case StmtAssign:
			ident = nestedStmt.Idents[0]
		case StmtReceive:
			ident = nestedStmt.Idents[1]
		default:
			nestedStmt.calculatePrescopes(prescope)
			continue
		}
		newPrescope := map[string]Token{}
		for k, v := range prescope {
			newPrescope[k] = v
		}
		newPrescope[ident.T] = ident
		prescope = newPrescope
		nestedStmt.calculatePrescopes(prescope)
	}
}

func (stmt *Stmt) calculateLoopScopes(loopScope map[string]Token) {
	stmt.loopScope = loopScope
	switch stmt.Type {
	case StmtFork:
		loopScope = map[string]Token{}
		loopScope[stmt.Idents[0].T] = stmt.Token
	case StmtLoop, StmtSend:
		if len(stmt.Idents) > 0 {
			newLoopScope := map[string]Token{}
			for k, v := range loopScope {
				newLoopScope[k] = v
			}
			newLoopScope[stmt.Idents[0].T] = stmt.Token
			loopScope = newLoopScope
		}
	}
	for _, nestedStmt := range stmt.Stmts {
		nestedStmt.calculateLoopScopes(loopScope)
	}
}

func (stmt *Stmt) calculateForkScopes(forkScope map[string]*Stmt) {
	if len(stmt.Stmts) == 0 {
		stmt.forkScope = forkScope
		return
	}
	newForkScope := map[string]*Stmt{}
	for k, v := range forkScope {
		newForkScope[k] = v
	}
	for _, nestedStmt := range stmt.Stmts {
		switch nestedStmt.Type {
		case StmtFork:
			newForkScope[nestedStmt.Idents[0].T] = nestedStmt
		}
	}
	for _, nestedStmt := range stmt.Stmts {
		nestedStmt.calculateForkScopes(newForkScope)
	}
}

func (stmt *Stmt) calculateQueueScopes(queueScope map[string]Token) {
	stmt.queueScope = queueScope
	switch stmt.Type {
	case StmtFork:
		queueScope = map[string]Token{}
		queueScope[stmt.Idents[0].T] = stmt.Token
	}
	for _, nestedStmt := range stmt.Stmts {
		nestedStmt.calculateQueueScopes(queueScope)
		switch nestedStmt.Type {
		case StmtFork:
			newQueueScope := map[string]Token{}
			newQueueScope[nestedStmt.Idents[0].T] = nestedStmt.Token
			for k, v := range queueScope {
				newQueueScope[k] = v
			}
			queueScope = newQueueScope
		}
	}

}

func (stmt *Stmt) check() error {
	switch stmt.Type {
	case StmtAssign:
		if len(stmt.Stmts) != 0 || len(stmt.Idents) != 1 || stmt.Expr == nil {
			panic("check:StmtAssign")
		}
		if tok, ok := stmt.varScope[stmt.Idents[0].T]; ok {
			return fmt.Errorf("%s: redeclaration of %s originally declared at %s", stmt.Idents[0].Loc(), stmt.Idents[0].T, tok.Loc())
		}
	case StmtBreak:
		if len(stmt.Stmts) != 0 || len(stmt.Idents) > 1 {
			panic("check:StmtBreak")
		}
		if len(stmt.Idents) > 0 {
			if _, ok := stmt.loopScope[stmt.Idents[0].T]; !ok {
				return fmt.Errorf("%s: unknown loop %s", stmt.Idents[0].Loc(), stmt.Idents[0].T)
			}
		}
	case StmtContinue:
		if len(stmt.Stmts) != 0 || len(stmt.Idents) > 1 {
			panic("check:StmtContinue")
		}
		if len(stmt.Idents) > 0 {
			if _, ok := stmt.loopScope[stmt.Idents[0].T]; !ok {
				return fmt.Errorf("%s: unknown loop %s", stmt.Idents[0].Loc(), stmt.Idents[0].T)
			}
		}
	case StmtFork:
		if len(stmt.Idents) == 0 || len(stmt.Idents) > 2 || stmt.Expr != nil {
			panic("check:StmtFork")
		}
		if tok, ok := stmt.loopScope[stmt.Idents[0].T]; ok {
			return fmt.Errorf("%s: redeclaration of %s originally declared at %s", stmt.Idents[0].Loc(), stmt.Idents[0].T, tok.Loc())
		}
		if forkStmt, ok := stmt.forkScope[stmt.Idents[0].T]; ok {
			return fmt.Errorf("%s: redeclaration of %s originally declared at %s", stmt.Idents[0].Loc(), stmt.Idents[0].T, forkStmt.Token.Loc())
		}
		if tok, ok := stmt.queueScope[stmt.Idents[0].T]; ok {
			return fmt.Errorf("%s: redeclaration of %s originally declared at %s", stmt.Idents[0].Loc(), stmt.Idents[0].T, tok.Loc())
		}
		if len(stmt.Idents) == 2 {
			if len(stmt.Stmts) > 0 {
				panic("check:StmtFork")
				
			}
			if forkStmt, ok := stmt.forkScope[stmt.Idents[1].T]; !ok {
				return fmt.Errorf("%s: unknown queue %s", stmt.Idents[1].Loc(), stmt.Idents[1].T)
			} else if len(forkStmt.Idents) != 1 {
				return fmt.Errorf("%s: invalid + queue %s declared at %s", stmt.Idents[1].Loc(), stmt.Idents[1].T, forkStmt.Token.Loc())
			}
		}
	case StmtLoop:
		if len(stmt.Idents) > 1 || stmt.Expr != nil {
			panic("check:StmtLoop")
		}
		if len(stmt.Idents) > 0 {
			if tok, ok := stmt.loopScope[stmt.Idents[0].T]; ok {
				return fmt.Errorf("%s: redeclaration of %s originally declared at %s", stmt.Idents[0].Loc(), stmt.Idents[0].T, tok.Loc())
			}
			if tok, ok := stmt.queueScope[stmt.Idents[0].T]; ok {
				return fmt.Errorf("%s: redeclaration of %s originally declared at %s", stmt.Idents[0].Loc(), stmt.Idents[0].T, tok.Loc())
			}
		}
	case StmtReceive:
		if len(stmt.Stmts) > 0 || len(stmt.Idents) < 2 || len(stmt.Idents) > 3 || stmt.Expr != nil {
			panic("check:StmtReceive")
		}
		if _, ok := stmt.queueScope[stmt.Idents[0].T]; !ok {
			return fmt.Errorf("%s: unknown queue %s", stmt.Idents[0].Loc(), stmt.Idents[0].T)
		}
		if tok, ok := stmt.varScope[stmt.Idents[1].T]; ok {
			return fmt.Errorf("%s: redeclaration of %s originally declared at %s", stmt.Idents[1].Loc(), stmt.Idents[1].T, tok.Loc())
		}
		if len(stmt.Idents) > 2 {
			if _, ok := stmt.loopScope[stmt.Idents[2].T]; !ok {
				return fmt.Errorf("%s: unknown loop %s", stmt.Idents[2].Loc(), stmt.Idents[2].T)
			}
		}
	case StmtSend:
		if len(stmt.Idents) != 1 || stmt.Expr == nil {
			panic("check:StmtSend")
		}
		if _, ok := stmt.queueScope[stmt.Idents[0].T]; !ok {
			return fmt.Errorf("%s: unknown queue %s", stmt.Idents[0].Loc(), stmt.Idents[0].T)
		}
	default:
		panic("check:stmt.Type")
	}
	if err := stmt.Expr.check(stmt); err != nil {
		return err
	}
	for _, nestedStmt := range stmt.Stmts {
		if err := nestedStmt.check(); err != nil {
			return err
		}
	}
	return nil
}

func (stmt *Stmt) Unparse(b *strings.Builder) {
	if stmt.Type != StmtLoop || len(stmt.Idents) != 0 || stmt.Expr != nil {
		panic("Unparse")
	}
	for _, s := range stmt.Stmts {
		s.unparse(b, "")
	}
}

func (stmt *Stmt) unparse(b *strings.Builder, indent string) {
	const addIndent = "  "
	switch stmt.Type {
	case StmtAssign:
		b.WriteString(indent)
		b.WriteString(stmt.Idents[0].T)
		b.WriteString(" = ")
		stmt.Expr.Unparse(b)
		b.WriteString(".\n")
	case StmtBreak:
		b.WriteString(indent)
		if len(stmt.Idents) > 0 {
			b.WriteString(stmt.Idents[0].T)
			b.WriteString(" ")
		}
		b.WriteString("break")
		if stmt.Expr != nil {
			b.WriteString(" ")
			stmt.Expr.Unparse(b)
		}
		b.WriteString(".\n")
	case StmtContinue:
		b.WriteString(indent)
		if len(stmt.Idents) > 0 {
			b.WriteString(stmt.Idents[0].T)
			b.WriteString(" ")
		}
		b.WriteString("continue")
		if stmt.Expr != nil {
			b.WriteString(" ")
			stmt.Expr.Unparse(b)
		}
		b.WriteString(".\n")
	case StmtFork:
		b.WriteString(indent)
		b.WriteString(stmt.Idents[0].T)
		b.WriteString("+")
		if len(stmt.Idents) > 1 {
			b.WriteString(stmt.Idents[1].T)
			b.WriteString(".\n")
		} else {
			b.WriteString("{\n")
			for _, s := range stmt.Stmts {
				s.unparse(b, indent+addIndent)
			}
			b.WriteString(indent)
			b.WriteString("}\n")
		}
	case StmtLoop:
		b.WriteString(indent)
		if len(stmt.Idents) > 0 {
			b.WriteString(stmt.Idents[0].T)
			b.WriteString(" ")
		}
		b.WriteString("{\n")
		for _, s := range stmt.Stmts {
			s.unparse(b, indent+addIndent)
		}
		b.WriteString(indent)
		b.WriteString("}\n")
	case StmtReceive:
		b.WriteString(indent)
		b.WriteString(stmt.Idents[0].T)
		b.WriteString(" > ")
		b.WriteString(stmt.Idents[1].T)
		if len(stmt.Idents) > 2 {
			b.WriteString(" ")
			b.WriteString(stmt.Idents[2].T)
		}
		b.WriteString(".\n")
	case StmtSend:
		b.WriteString(indent)
		b.WriteString(stmt.Idents[0].T)
		b.WriteString(" < ")
		stmt.Expr.Unparse(b)
		if len(stmt.Stmts) == 0 {
			b.WriteString(".\n")
		} else {
			b.WriteString(" {\n")
			for _, s := range stmt.Stmts {
				s.unparse(b, indent+addIndent)
			}
			b.WriteString(indent)
			b.WriteString("}\n")
		}
	}
}

func parseExpr(tokenizer *Tokenizer) (*Expr, error) {
	var expr *Expr
	for {
		tok, ok := tokenizer.Token()
		if !ok {
			return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
		}
		switch tok.T {
		case ".", ")", "{":
			if expr == nil {
				return nil, fmt.Errorf("%s: unexpected token: %s", tok.Loc(), tok.T)
			}
			return expr, nil
		case "}", "+", "<", ">", "=":
			return nil, fmt.Errorf("%s: unexpected token: %s", tok.Loc(), tok.T)
		}
		leftExpr, err := parseLeftExpr(tokenizer)
		if err != nil {
			return nil, err
		}
		if expr == nil {
			expr = leftExpr
		} else {
			expr = &Expr{
				Type:     ExprNand,
				Token:    expr.Token,
				SubExpr1: expr,
				SubExpr2: leftExpr,
			}
		}
	}
}

func parseLeftExpr(tokenizer *Tokenizer) (*Expr, error) {
	tok, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	if tok.T == "(" {
		tokenizer.Next()
		expr, err := parseExpr(tokenizer)
		if err != nil {
			return nil, err
		}
		endTok, ok := tokenizer.Token()
		if !ok || endTok.T != ")" {
			return nil, fmt.Errorf("%s: unmatched (", tok.Loc())
		}
		tokenizer.Next()
		return expr, nil
	}
	if !tok.IsIdent() {
		return nil, fmt.Errorf("%s: unexpected token: %s", tok.Loc(), tok.T)
	}
	ident := tok
	tokenizer.Next()
	tok, ok = tokenizer.Token()
	if !ok || tok.T != "<" {
		return &Expr{
			Type:  ExprVar,
			Token: ident,
		}, nil
	}
	tokenizer.Next()
	expr, err := parseExpr(tokenizer)
	if err != nil {
		return nil, err
	}
	return &Expr{
		Type:     ExprPrevVar,
		Token:    ident,
		SubExpr1: expr,
	}, nil
}

func (expr *Expr) check(stmt *Stmt) error {
	if expr == nil {
		return nil
	}
	switch expr.Type {
	case ExprNand:
		if expr.SubExpr1 == nil || expr.SubExpr2 == nil {
			panic("check.ExprNand")
		}
	case ExprPrevVar:
		if expr.SubExpr1 == nil || expr.SubExpr2 != nil {
			panic("check.ExprPrevVar")
		}
		if _, ok := stmt.varScope[expr.Token.T]; !ok {
			if _, ok := stmt.prescope[expr.Token.T]; !ok {
				return fmt.Errorf("%s: unknown variable %s", expr.Token.Loc(), stmt.Token.T)
			}
		}
	case ExprVar:
		if expr.SubExpr1 != nil || expr.SubExpr2 != nil {
			panic("check.ExprPrevVar")
		}
		if _, ok := stmt.varScope[expr.Token.T]; !ok {
			return fmt.Errorf("%s: unknown variable %s", expr.Token.Loc(), stmt.Token.T)
		}
	default:
		panic("check:expr.Type")
	}
	if err := expr.SubExpr1.check(stmt); err != nil {
		return err
	}
	if err := expr.SubExpr2.check(stmt); err != nil {
		return err
	}
	return nil
}

func (expr *Expr) Unparse(b *strings.Builder) {
	switch expr.Type {
	case ExprNand:
		needsParens := expr.SubExpr1.Type == ExprPrevVar
		if needsParens {
			b.WriteString("(")
		}
		expr.SubExpr1.Unparse(b)
		if needsParens {
			b.WriteString(") ")
		} else {
			b.WriteString(" ")
		}
		needsParens = expr.SubExpr2.Type == ExprNand
		if needsParens {
			b.WriteString("(")
		}
		expr.SubExpr2.Unparse(b)
		if needsParens {
			b.WriteString(")")
		}
	case ExprPrevVar:
		b.WriteString(expr.Token.T)
		b.WriteString(" < ")
		expr.SubExpr1.Unparse(b)
	case ExprVar:
		b.WriteString(expr.Token.T)
	}
}
