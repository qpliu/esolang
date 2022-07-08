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
	StmtLoop
	StmtReceive
	StmtSend
)

type ExprType int

const (
	ExprVar ExprType = iota
	ExprOldVar
	ExprNand
)

type Stmt struct {
	Type     StmtType
	scope    map[string]bool
	prescope map[string]bool

	Stmts       []*Stmt
	Identifiers []string
	Expr        *Expr

	Declarations map[string]bool
}

type Expr struct {
	Type ExprType

	Identifier string
	Exprs      []*Expr
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
	stmt.calculateDeclarations()
	scope := map[string]bool{"0": true}
	stmt.calculateScopes(scope)
	prescope := map[string]bool{}
	stmt.calculatePrescopes(prescope)
	queues := map[string]bool{"left": true, "right": true, "io": true}
	loopScopes := map[string]bool{}
	return stmt, stmt.check(queues, loopScopes)
}

func parseStmt(tokenizer *Tokenizer) (*Stmt, error) {
	tok, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("unexpected EOF")
	}
	switch tok {
	case "{":
		return parseStmtLoop("", tokenizer)
	case "break":
		return parseStmtExpr("", StmtBreak, tokenizer)
	case "continue":
		return parseStmtExpr("", StmtContinue, tokenizer)
	}
	if !IsIdentifier(tok) {
		return nil, fmt.Errorf("unexpected token: %s", tok)
	}
	tokenizer.Next()
	identifier := tok
	tok, ok = tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("unexpected EOF")
	}
	switch tok {
	case "=":
		return parseStmtExpr(identifier, StmtAssign, tokenizer)
	case "<":
		return parseStmtExpr(identifier, StmtSend, tokenizer)
	case ">":
		return parseStmtReceive(identifier, tokenizer)
	case "{":
		return parseStmtLoop(identifier, tokenizer)
	case "break":
		return parseStmtExpr(identifier, StmtBreak, tokenizer)
	case "continue":
		return parseStmtExpr(identifier, StmtContinue, tokenizer)
	default:
		return nil, fmt.Errorf("unexpected token: %s", tok)
	}
}

func parseStmtLoop(identifier string, tokenizer *Tokenizer) (*Stmt, error) {
	tokenizer.Next()
	stmts := []*Stmt{}
	for {
		tok, ok := tokenizer.Token()
		if !ok {
			return nil, fmt.Errorf("unexpected EOF")
		}
		if tok == "}" {
			tokenizer.Next()
			stmt := &Stmt{
				Type:  StmtLoop,
				Stmts: stmts,
			}
			if identifier != "" {
				stmt.Identifiers = []string{identifier}
			}
			return stmt, nil
		}
		if stmt, err := parseStmt(tokenizer); err != nil {
			return nil, err
		} else {
			stmts = append(stmts, stmt)
		}
	}
}

func parseStmtReceive(identifier string, tokenizer *Tokenizer) (*Stmt, error) {
	tokenizer.Next()
	tok, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("unexpected EOF")
	}
	if !IsIdentifier(tok) {
		return nil, fmt.Errorf("unexpected token: %s", tok)
	}
	varName := tok
	tokenizer.Next()
	tok, ok = tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("unexpected EOF")
	}
	if tok != "." {
		return nil, fmt.Errorf("unexpected token: %s", tok)
	}
	tokenizer.Next()
	return &Stmt{
		Type:        StmtReceive,
		Identifiers: []string{identifier, varName},
	}, nil
}

func parseStmtExpr(identifier string, stmtType StmtType, tokenizer *Tokenizer) (*Stmt, error) {
	tokenizer.Next()
	expr, err := parseExpr(tokenizer)
	if err != nil {
		return nil, err
	}
	tok, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("unexpected EOF")
	}
	if tok != "." {
		return nil, fmt.Errorf("unexpected token: %s", tok)
	}
	tokenizer.Next()
	stmt := &Stmt{
		Type: stmtType,
		Expr: expr,
	}
	if identifier != "" {
		stmt.Identifiers = []string{identifier}
	}
	return stmt, nil
}

func parseExpr(tokenizer *Tokenizer) (*Expr, error) {
	var expr *Expr
	for {
		tok, ok := tokenizer.Token()
		if !ok || tok == "." || tok == ")" {
			if expr == nil {
				return nil, fmt.Errorf("invalid expression")
			}
			return expr, nil
		}
		leftExpr, err := parseLeftExpr(tokenizer)
		if err != nil {
			return nil, err
		}
		if expr == nil {
			expr = leftExpr
		} else {
			expr = &Expr{
				Type:  ExprNand,
				Exprs: []*Expr{expr, leftExpr},
			}
		}
	}
}

func parseLeftExpr(tokenizer *Tokenizer) (*Expr, error) {
	tok, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("unexpected EOF")
	}
	if tok == "(" {
		tokenizer.Next()
		expr, err := parseExpr(tokenizer)
		if err != nil {
			return nil, err
		}
		tok, ok = tokenizer.Token()
		if !ok || tok != ")" {
			return nil, fmt.Errorf("invalid expression")
		}
		tokenizer.Next()
		return expr, nil
	}
	if !IsIdentifier(tok) {
		return nil, fmt.Errorf("unexpected token: %s", tok)
	}
	identifier := tok
	tokenizer.Next()
	tok, ok = tokenizer.Token()
	if !ok || tok != "<" {
		return &Expr{
			Type:       ExprVar,
			Identifier: identifier,
		}, nil
	}
	tokenizer.Next()
	expr, err := parseExpr(tokenizer)
	if err != nil {
		return nil, err
	}
	return &Expr{
		Type:       ExprOldVar,
		Identifier: identifier,
		Exprs:      []*Expr{expr},
	}, nil
}

func (stmt *Stmt) calculateDeclarations() {
	if stmt.Type != StmtLoop {
		return
	}
	stmt.Declarations = map[string]bool{}
	for _, s := range stmt.Stmts {
		s.calculateDeclarations()
		switch s.Type {
		case StmtAssign:
			stmt.Declarations[s.Identifiers[0]] = true
		case StmtReceive:
			stmt.Declarations[s.Identifiers[1]] = true
		}
	}
}

func (stmt *Stmt) calculateScopes(scopes map[string]bool) {
	stmt.scope = scopes
	if stmt.Type != StmtLoop {
		return
	}
	for _, s := range stmt.Stmts {
		s.calculateScopes(scopes)
		switch s.Type {
		case StmtAssign:
			newScopes := map[string]bool{}
			newScopes[s.Identifiers[0]] = true
			for k, v := range scopes {
				newScopes[k] = v
			}
			scopes = newScopes
		case StmtReceive:
			newScopes := map[string]bool{}
			newScopes[s.Identifiers[1]] = true
			for k, v := range scopes {
				newScopes[k] = v
			}
			scopes = newScopes
		}
	}
}

func (stmt *Stmt) calculatePrescopes(prescopes map[string]bool) map[string]bool {
	outerContinues := map[string]bool{}
	stmt.prescope = prescopes
	switch stmt.Type {
	case StmtContinue:
		if len(stmt.Identifiers) > 0 {
			outerContinues[stmt.Identifiers[0]] = true
		}
		return outerContinues
	case StmtLoop:
		for i := len(stmt.Stmts) - 1; i >= 0; i-- {
			s := stmt.Stmts[i]
			continues := s.calculatePrescopes(prescopes)
			for k, v := range continues {
				outerContinues[k] = v
			}
			if len(stmt.Identifiers) > 0 && continues[stmt.Identifiers[0]] {
				delete(outerContinues, stmt.Identifiers[0])
				prescopes = map[string]bool{}
			}
			switch s.Type {
			case StmtContinue:
				if len(s.Identifiers) == 0 {
					prescopes = map[string]bool{}
				}
			case StmtAssign:
				newPrescopes := map[string]bool{}
				newPrescopes[s.Identifiers[0]] = true
				for k, v := range prescopes {
					newPrescopes[k] = v
				}
				prescopes = newPrescopes
			case StmtReceive:
				newPrescopes := map[string]bool{}
				newPrescopes[s.Identifiers[1]] = true
				for k, v := range prescopes {
					newPrescopes[k] = v
				}
				prescopes = newPrescopes
			}
		}
		return outerContinues
	default:
		return outerContinues
	}
}

func (stmt *Stmt) check(queues, loopScopes map[string]bool) error {
	switch stmt.Type {
	case StmtAssign:
		if len(stmt.Stmts) != 0 || len(stmt.Identifiers) != 1 || stmt.Expr == nil {
			panic("internal error: StmtAssign")
		}
		if stmt.scope[stmt.Identifiers[0]] {
			return fmt.Errorf("reassignment: %s", stmt.Identifiers[0])
		}
		if err := stmt.Expr.check(stmt.scope, stmt.prescope); err != nil {
			return err
		}
	case StmtBreak:
		if len(stmt.Stmts) != 0 || len(stmt.Identifiers) > 1 || stmt.Expr == nil {
			panic("internal error: StmtBreak")
		}
		if len(stmt.Identifiers) > 0 && !loopScopes[stmt.Identifiers[0]] {
			return fmt.Errorf("break: unknown loop identifier: %s", stmt.Identifiers[0])
		}
		if err := stmt.Expr.check(stmt.scope, stmt.prescope); err != nil {
			return err
		}
	case StmtContinue:
		if len(stmt.Stmts) != 0 || len(stmt.Identifiers) > 1 || stmt.Expr == nil {
			panic("internal error: StmtContinue")
		}
		if len(stmt.Identifiers) > 0 && !loopScopes[stmt.Identifiers[0]] {
			return fmt.Errorf("continue: unknown loop identifier: %s", stmt.Identifiers[0])
		}
		if err := stmt.Expr.check(stmt.scope, stmt.prescope); err != nil {
			return err
		}
	case StmtLoop:
		if len(stmt.Identifiers) > 1 || stmt.Expr != nil {
			panic("internal error: StmtLoop")
		}
		if len(stmt.Identifiers) > 0 {
			if loopScopes[stmt.Identifiers[0]] {
				return fmt.Errorf("duplicate loop identifier: %s", stmt.Identifiers[0])
			}
			loopScopes[stmt.Identifiers[0]] = true
		}
		for _, innerStmt := range stmt.Stmts {
			if err := innerStmt.check(queues, loopScopes); err != nil {
				return err
			}
		}
		if len(stmt.Identifiers) > 0 {
			delete(loopScopes, stmt.Identifiers[0])
		}
	case StmtReceive:
		if len(stmt.Stmts) != 0 || len(stmt.Identifiers) != 2 || stmt.Expr != nil {
			panic("internal error: StmtReceive")
		}
		if !queues[stmt.Identifiers[0]] {
			return fmt.Errorf("invalid receive from queue: %s", stmt.Identifiers[0])
		}
		if stmt.scope[stmt.Identifiers[1]] {
			return fmt.Errorf("reassignment on receive: %s", stmt.Identifiers[1])
		}
	case StmtSend:
		if len(stmt.Stmts) != 0 || len(stmt.Identifiers) != 1 || stmt.Expr == nil {
			panic("internal error: StmtSend")
		}
		if !queues[stmt.Identifiers[0]] {
			return fmt.Errorf("invalid send to queue: %s", stmt.Identifiers[0])
		}
		if err := stmt.Expr.check(stmt.scope, stmt.prescope); err != nil {
			return err
		}
	default:
		panic("internal error: stmt.Type")
	}
	return nil
}

func IsIdentifier(tok string) bool {
	switch tok {
	case ".", "=", "<", ">", "(", ")", "{", "}", "break", "continue":
		return false
	default:
		return true
	}
}

func (expr Expr) check(scope, prescope map[string]bool) error {
	switch expr.Type {
	case ExprVar:
		if expr.Identifier == "" || len(expr.Exprs) != 0 {
			panic("internal error: ExprVar")
		}
		if !scope[expr.Identifier] {
			return fmt.Errorf("unknown variable: %s", expr.Identifier)
		}
	case ExprOldVar:
		if expr.Identifier == "" || len(expr.Exprs) != 1 {
			panic("internal error: ExprOldVar")
		}
		if !prescope[expr.Identifier] && !scope[expr.Identifier] {
			return fmt.Errorf("unknown variable: %s", expr.Identifier)
		}
		if err := expr.Exprs[0].check(scope, prescope); err != nil {
			return err
		}
	case ExprNand:
		if expr.Identifier != "" || len(expr.Exprs) != 2 {
			panic("internal error: ExprNand")
		}
		if err := expr.Exprs[0].check(scope, prescope); err != nil {
			return err
		}
		if err := expr.Exprs[1].check(scope, prescope); err != nil {
			return err
		}
	default:
		panic("unparse:expr.Type")
	}
	return nil
}

func (stmt Stmt) Unparse(b *strings.Builder) {
	stmt.unparse(b, "", true)
}

func (stmt Stmt) unparse(b *strings.Builder, indent string, inImplicitLoop bool) {
	switch stmt.Type {
	case StmtAssign:
		if len(stmt.Stmts) != 0 || len(stmt.Identifiers) != 1 || stmt.Expr == nil {
			panic("unparse:StmtAssign")
		}
		b.WriteString(indent)
		b.WriteString(stmt.Identifiers[0])
		b.WriteString(" = ")
		stmt.Expr.Unparse(b)
		b.WriteString(".\n")
	case StmtBreak:
		if len(stmt.Stmts) != 0 || len(stmt.Identifiers) > 1 || stmt.Expr == nil {
			panic("unparse:StmtBreak")
		}
		b.WriteString(indent)
		if len(stmt.Identifiers) > 0 {
			b.WriteString(stmt.Identifiers[0])
			b.WriteString(" ")
		}
		b.WriteString("break ")
		stmt.Expr.Unparse(b)
		b.WriteString(".\n")
	case StmtContinue:
		if len(stmt.Stmts) != 0 || len(stmt.Identifiers) > 1 || stmt.Expr == nil {
			panic("unparse:StmtContinue")
		}
		b.WriteString(indent)
		if len(stmt.Identifiers) > 0 {
			b.WriteString(stmt.Identifiers[0])
			b.WriteString(" ")
		}
		b.WriteString("continue ")
		stmt.Expr.Unparse(b)
		b.WriteString(".\n")
	case StmtLoop:
		if len(stmt.Identifiers) > 1 || stmt.Expr != nil {
			panic("unparse:StmtLoop")
		}
		if inImplicitLoop {
			if len(stmt.Identifiers) != 0 {
				panic("unparse:StmtLoop")
			}
			for _, innerStmt := range stmt.Stmts {
				innerStmt.unparse(b, indent, false)
			}
		} else {
			innerIndent := indent + "  "
			b.WriteString(indent)
			if len(stmt.Identifiers) > 0 {
				b.WriteString(stmt.Identifiers[0])
				b.WriteString(" ")
			}
			b.WriteString("{\n")
			for _, innerStmt := range stmt.Stmts {
				innerStmt.unparse(b, innerIndent, false)
			}
			b.WriteString(indent)
			b.WriteString("}\n")
		}
	case StmtReceive:
		if len(stmt.Stmts) != 0 || len(stmt.Identifiers) != 2 || stmt.Expr != nil {
			panic("unparse:StmtReceive")
		}
		b.WriteString(indent)
		b.WriteString(stmt.Identifiers[0])
		b.WriteString(" > ")
		b.WriteString(stmt.Identifiers[1])
		b.WriteString(".\n")
	case StmtSend:
		if len(stmt.Stmts) != 0 || len(stmt.Identifiers) != 1 || stmt.Expr == nil {
			panic("unparse:StmtSend")
		}
		b.WriteString(indent)
		b.WriteString(stmt.Identifiers[0])
		b.WriteString(" < ")
		stmt.Expr.Unparse(b)
		b.WriteString(".\n")
	default:
		panic("unparse:stmt.Type")
	}
}

func (expr Expr) Unparse(b *strings.Builder) {
	switch expr.Type {
	case ExprVar:
		if expr.Identifier == "" || len(expr.Exprs) != 0 {
			panic("unparse.ExprVar")
		}
		b.WriteString(expr.Identifier)
	case ExprOldVar:
		if expr.Identifier == "" || len(expr.Exprs) != 1 {
			panic("unparse.ExprOldVar")
		}
		b.WriteString(expr.Identifier)
		b.WriteString(" < ")
		expr.Exprs[0].Unparse(b)
	case ExprNand:
		if expr.Identifier != "" || len(expr.Exprs) != 2 {
			panic("unparse.Nand")
		}
		needsParens := expr.Exprs[0].Type == ExprOldVar
		if needsParens {
			b.WriteString("(")
		}
		expr.Exprs[0].Unparse(b)
		if needsParens {
			b.WriteString(") ")
		} else {
			b.WriteString(" ")
		}
		needsParens = expr.Exprs[1].Type == ExprNand
		if needsParens {
			b.WriteString("(")
		}
		expr.Exprs[1].Unparse(b)
		if needsParens {
			b.WriteString(")")
		}
	default:
		panic("unparse:expr.Type")
	}
}
