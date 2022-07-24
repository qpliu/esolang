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
	StmtMessage
	StmtSpawn
)

type ArmType int

const (
	ArmRecv ArmType = iota
	ArmSend
)

type Routine struct {
	Name   Token
	Params []Token
	Stmt   *Stmt
}

type Stmt struct {
	Type StmtType
	Loc  string

	Guards []Guard
	LoopID []Token
	Vars   []Token
	Exprs  []Token
	Stmts  []*Stmt
	Arms   []*Arm
}

type Guard struct {
	Exprs []Token
	Equal bool
}

type Arm struct {
	Type ArmType

	Guards []Guard
	Vars   []Token
	Exprs  []Token
	Stmts  []*Stmt
}

func Parse(tokenizer *Tokenizer) (map[string]*Routine, error) {
	result := map[string]*Routine{}
	for {
		if _, ok := tokenizer.Token(); !ok {
			break
		}
		if rout, err := parseRoutine(tokenizer); err != nil {
			return nil, err
		} else if other, ok := result[rout.Name.T]; ok {
			return nil, fmt.Errorf("%s: duplicate routine %s, previously defined at %s", rout.Name.Loc(), rout.Name.T, other.Name.Loc())
		} else {
			result[rout.Name.T] = rout
		}
	}
	for _, rout := range result {
		if err := rout.check(result); err != nil {
			return nil, err
		}
	}
	return result, nil
}

func parseRoutine(tokenizer *Tokenizer) (*Routine, error) {
	tok, ok := tokenizer.Token()
	if !ok {
		panic("parseRoutine")
	}
	if !tok.IsIdent() {
		return nil, fmt.Errorf("%s: unexpected token %s", tok.Loc(), tok.T)
	}
	rout := &Routine{Name: tok, Stmt: &Stmt{Type: StmtLoop}}
	for {
		tokenizer.Next()
		tok, ok = tokenizer.Token()
		if !ok {
			return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
		}
		if tok.IsIdent() {
			for _, p := range rout.Params {
				if p.T == tok.T {
					return nil, fmt.Errorf("%s: duplicate parameter %s, previously at %s", tok.Loc(), tok.T, p.Loc())
				}
			}
			rout.Params = append(rout.Params, tok)
		} else if tok.T != "{" {
			return nil, fmt.Errorf("%s: unexpected token %s", tok.Loc(), tok.T)
		} else {
			break
		}
	}
	rout.Stmt.Loc = tok.Loc()
	if body, err := parseBody(tokenizer); err != nil {
		return nil, err
	} else {
		rout.Stmt.Stmts = body
	}
	return rout, nil
}

func parseBody(tokenizer *Tokenizer) ([]*Stmt, error) {
	tokenizer.Next()
	result := []*Stmt{}
	for {
		if tok, ok := tokenizer.Token(); !ok {
			return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
		} else if tok.T == "}" {
			tokenizer.Next()
			return result, nil
		}
		if stmt, err := parseStmt(tokenizer); err != nil {
			return nil, err
		} else {
			result = append(result, stmt)
		}
	}
}

func parseStmt(tokenizer *Tokenizer) (*Stmt, error) {
	guards, err := parseGuards(tokenizer)
	if err != nil {
		return nil, err
	}
	tok1, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	switch tok1.T {
	case "break":
		tokenizer.Next()
		return &Stmt{
			Type:   StmtBreak,
			Loc:    tok1.Loc(),
			Guards: guards,
		}, nil
	case "continue":
		tokenizer.Next()
		return &Stmt{
			Type:   StmtContinue,
			Loc:    tok1.Loc(),
			Guards: guards,
		}, nil
	case "[":
		return parseMessage(tokenizer, tok1.Loc(), guards, []Token{})
	case "{":
		if stmts, err := parseBody(tokenizer); err != nil {
			return nil, err
		} else {
			return &Stmt{
				Type:   StmtLoop,
				Loc:    tok1.Loc(),
				Guards: guards,
				Stmts:  stmts,
			}, nil
		}
	}
	if !tok1.IsIdent() {
		return nil, fmt.Errorf("%s: unexpected token %s", tok1.Loc(), tok1.T)
	}

	tokenizer.Next()
	tok2, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	switch tok2.T {
	case "break":
		tokenizer.Next()
		return &Stmt{
			Type:   StmtBreak,
			Loc:    tok1.Loc(),
			LoopID: []Token{tok1},
			Guards: guards,
		}, nil
	case "continue":
		tokenizer.Next()
		return &Stmt{
			Type:   StmtContinue,
			Loc:    tok1.Loc(),
			LoopID: []Token{tok1},
			Guards: guards,
		}, nil
	case "[":
		return parseMessage(tokenizer, tok1.Loc(), guards, []Token{tok1})
	case "{":
		if stmts, err := parseBody(tokenizer); err != nil {
			return nil, err
		} else {
			return &Stmt{
				Type:   StmtLoop,
				Loc:    tok1.Loc(),
				Guards: guards,
				LoopID: []Token{tok1},
				Stmts:  stmts,
			}, nil
		}
	case "<":
		tokenizer.Next()
		if tok3, ok := tokenizer.Token(); !ok {
			return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
		} else if tok3.IsExpr() {
			tokenizer.Next()
			return &Stmt{
				Type:   StmtAssign,
				Loc:    tok1.Loc(),
				Guards: guards,
				Vars:   []Token{tok1},
				Exprs:  []Token{tok3},
			}, nil
		} else if tok3.T != "[" {
			return nil, fmt.Errorf("%s: unexpected token %s", tok3.Loc(), tok3.T)
		} else {
			stmt := &Stmt{
				Type:   StmtSpawn,
				Loc:    tok1.Loc(),
				Guards: guards,
				Vars:   []Token{tok1},
			}
			for {
				tokenizer.Next()
				if tok, ok := tokenizer.Token(); !ok {
					return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
				} else if tok.T == "]" {
					if len(stmt.Exprs) == 0 {
						return nil, fmt.Errorf("%s: missing routine", tok.Loc())
					}
					tokenizer.Next()
					return stmt, nil
				} else if !tok.IsExpr() || (len(stmt.Exprs) == 0 && !tok.IsIdent()) {
					return nil, fmt.Errorf("%s: unexpected token %s", tok.Loc(), tok.T)
				} else {
					stmt.Exprs = append(stmt.Exprs, tok)
				}
			}
		}
	default:
		return nil, fmt.Errorf("%s: unexpected token %s", tok2.Loc(), tok2.T)
	}
}

func parseGuards(tokenizer *Tokenizer) ([]Guard, error) {
	result := []Guard{}
	for {
		left, ok := tokenizer.Token()
		if !ok {
			return result, nil
		}
		equal := true
		exprs := []Token{}
		switch left.T {
		case "=":
			equal = true
		case "!":
			equal = false
		default:
			if !left.IsExpr() {
				return result, nil
			}
			exprs = append(exprs, left)
			tokenizer.Next()
			op, ok := tokenizer.Token()
			if !ok {
				tokenizer.Push(left)
				return result, nil
			}
			switch op.T {
			case "=":
				equal = true
			case "!":
				equal = false
			default:
				tokenizer.Push(left)
				return result, nil
			}
		}
		tokenizer.Next()
		right, ok := tokenizer.Token()
		if !ok {
			return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
		}
		if !right.IsExpr() {
			return nil, fmt.Errorf("%s: unexpected token %s", right.Loc(), right.T)
		}
		tokenizer.Next()
		result = append(result, Guard{Exprs: append(exprs, right), Equal: equal})
	}
}

func parseMessage(tokenizer *Tokenizer, loc string, guards []Guard, loopID []Token) (*Stmt, error) {
	tokenizer.Next()
	arms := []*Arm{}
	for {
		if tok, ok := tokenizer.Token(); !ok {
			return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
		} else if tok.T == "]" {
			tokenizer.Next()
			return &Stmt{
				Type:   StmtMessage,
				Loc:    loc,
				Guards: guards,
				LoopID: loopID,
				Arms:   arms,
			}, nil
		}
		if arm, err := parseArm(tokenizer); err != nil {
			return nil, err
		} else {
			arms = append(arms, arm)
		}
	}
}

func parseArm(tokenizer *Tokenizer) (*Arm, error) {
	guards, err := parseGuards(tokenizer)
	if err != nil {
		return nil, err
	}
	arm := &Arm{
		Guards: guards,
	}

	tok1, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	if !tok1.IsExpr() {
		return nil, fmt.Errorf("%s: unexpected token %s", tok1.Loc(), tok1.T)
	}
	tokenizer.Next()
	tok2, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	tokenizer.Next()
	tok3, ok := tokenizer.Token()
	if !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	}
	tokenizer.Next()

	if tok2.T == "<" {
		if !tok3.IsExpr() {
			return nil, fmt.Errorf("%s: unexpected token %s", tok3.Loc(), tok3.T)
		}
		arm.Type = ArmSend
		arm.Exprs = append(arm.Exprs, tok1, tok3)
	} else if !tok1.IsIdent() {
		return nil, fmt.Errorf("%s: unexpected token %s", tok1.Loc(), tok1.T)
	} else if !tok2.IsIdent() {
		return nil, fmt.Errorf("%s: unexpected token %s", tok2.Loc(), tok2.T)
	} else if tok3.T != "<" {
		return nil, fmt.Errorf("%s: unexpected token %s", tok2.Loc(), tok2.T)
	} else {
		if tok1.T == tok2.T {
			return nil, fmt.Errorf("%s: sender assignment to %s must be distinct from message assignment at %s", tok2.Loc(), tok2.T, tok1.Loc())
		}
		arm.Type = ArmRecv
		arm.Vars = []Token{tok1, tok2}
		for {
			tokenizer.Next()
			if tok, ok := tokenizer.Token(); !ok {
				return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
			} else if tok.T == "{" {
				break
			} else if !tok.IsExpr() {
				return nil, fmt.Errorf("%s: unexpected token %s", tok.Loc(), tok.T)
			} else {
				arm.Exprs = append(arm.Exprs, tok)
			}
		}
	}

	if tok, ok := tokenizer.Token(); !ok {
		return nil, fmt.Errorf("%s: unexpected EOF", tokenizer.Loc())
	} else if tok.T != "{" {
		return nil, fmt.Errorf("%s: unexpected token %s", tok.Loc(), tok.T)
	}

	if stmts, err := parseBody(tokenizer); err != nil {
		return nil, err
	} else {
		arm.Stmts = stmts
	}
	return arm, nil
}

func (rout *Routine) check(prog map[string]*Routine) error {
	if rout.Stmt.Type != StmtLoop {
		panic("check:rout.Stmt.Type")
	}
	return rout.Stmt.check(prog, map[string]Token{rout.Name.T: rout.Name})
}

func (stmt *Stmt) check(prog map[string]*Routine, loopIDs map[string]Token) error {
	for _, guard := range stmt.Guards {
		if len(guard.Exprs) < 1 || len(guard.Exprs) > 2 {
			panic("check.stmt.Guards")
		}
	}
	for _, loopID := range stmt.LoopID {
		if !loopID.IsIdent() {
			panic("check:stmt.LoopID " + loopID.T)
		}
	}
	for _, v := range stmt.Vars {
		if !v.IsIdent() {
			panic("check:stmt.Vars " + v.T)
		}
	}
	for _, expr := range stmt.Exprs {
		if !expr.IsExpr() {
			panic("check:stmt.Exprs " + expr.T)
		}
	}
	switch stmt.Type {
	case StmtAssign:
		if len(stmt.LoopID) != 0 || len(stmt.Vars) != 1 || len(stmt.Exprs) != 1 || len(stmt.Stmts) != 0 || len(stmt.Arms) != 0 {
			panic("check:StmtAssign")
		}
	case StmtBreak:
		if len(stmt.LoopID) > 1 || len(stmt.Vars) != 0 || len(stmt.Exprs) != 0 || len(stmt.Stmts) != 0 || len(stmt.Arms) != 0 {
			panic("check:StmtBreak")
		}
		if len(stmt.LoopID) > 0 {
			if _, ok := loopIDs[stmt.LoopID[0].T]; !ok {
				return fmt.Errorf("%s: unknown loop %s", stmt.LoopID[0].Loc(), stmt.LoopID[0].T)
			}
		}
	case StmtContinue:
		if len(stmt.LoopID) > 1 || len(stmt.Vars) != 0 || len(stmt.Exprs) != 0 || len(stmt.Stmts) != 0 || len(stmt.Arms) != 0 {
			panic("check:StmtContinue")
		}
		if len(stmt.LoopID) > 0 {
			if _, ok := loopIDs[stmt.LoopID[0].T]; !ok {
				return fmt.Errorf("%s: unknown loop %s", stmt.LoopID[0].Loc(), stmt.LoopID[0].T)
			}
		}
	case StmtLoop:
		if len(stmt.LoopID) > 1 || len(stmt.Vars) != 0 || len(stmt.Exprs) != 0 || len(stmt.Arms) != 0 {
			panic("check:StmtLoop")
		}
		if len(stmt.LoopID) > 0 {
			if loopID, ok := loopIDs[stmt.LoopID[0].T]; ok {
				return fmt.Errorf("%s: duplicate loop identifier %s, previously at %s", stmt.LoopID[0].Loc(), stmt.LoopID[0].T, loopID.Loc())
			}
			loopIDs[stmt.LoopID[0].T] = stmt.LoopID[0]
			defer delete(loopIDs, stmt.LoopID[0].T)
		}
		for _, s := range stmt.Stmts {
			if err := s.check(prog, loopIDs); err != nil {
				return err
			}
		}
	case StmtMessage:
		if len(stmt.LoopID) > 1 || len(stmt.Vars) != 0 || len(stmt.Exprs) != 0 || len(stmt.Stmts) != 0 {
			panic("check:StmtMessage")
		}
		if len(stmt.LoopID) > 0 {
			if loopID, ok := loopIDs[stmt.LoopID[0].T]; ok {
				return fmt.Errorf("%s: duplicate loop identifier %s, previously at %s", stmt.LoopID[0].Loc(), stmt.LoopID[0].T, loopID.Loc())
			}
			loopIDs[stmt.LoopID[0].T] = stmt.LoopID[0]
			defer delete(loopIDs, stmt.LoopID[0].T)
		}
		for _, arm := range stmt.Arms {
			if err := arm.check(prog, loopIDs); err != nil {
				return err
			}
		}
	case StmtSpawn:
		if len(stmt.LoopID) != 0 || len(stmt.Vars) != 1 || len(stmt.Exprs) < 1 || !stmt.Exprs[0].IsIdent() || len(stmt.Stmts) != 0 || len(stmt.Arms) != 0 {
			panic("check:StmtSpawn")
		}
		if _, ok := prog[stmt.Exprs[0].T]; !ok {
			return fmt.Errorf("%s: unknown routine %s", stmt.Exprs[0].Loc(), stmt.Exprs[0].T)
		}
	default:
		panic("check:stmt.Type")
	}
	return nil
}

func (arm *Arm) check(prog map[string]*Routine, loopIDs map[string]Token) error {
	for _, guard := range arm.Guards {
		if len(guard.Exprs) < 1 || len(guard.Exprs) > 2 {
			panic("check.arm.Guards")
		}
	}
	switch len(arm.Vars) {
	case 0: // send
		if len(arm.Exprs) != 2 {
			panic("check.arm.Exprs")
		}
	case 2: // recv
	default:
		panic("check.arm.Vars")
	}
	for _, stmt := range arm.Stmts {
		if err := stmt.check(prog, loopIDs); err != nil {
			return err
		}
	}
	return nil
}

func (rout *Routine) Unparse(b *strings.Builder) {
	b.WriteString(rout.Name.T)
	for _, param := range rout.Params {
		b.WriteString(" ")
		b.WriteString(param.T)
	}
	b.WriteString(" ")
	rout.Stmt.unparse(b, "")
}

const addIndent = "  "

func (stmt *Stmt) unparse(b *strings.Builder, indent string) {
	b.WriteString(indent)
	for _, g := range stmt.Guards {
		g.unparse(b)
	}
	switch stmt.Type {
	case StmtAssign:
		b.WriteString(stmt.Vars[0].T)
		b.WriteString(" < ")
		b.WriteString(stmt.Exprs[0].T)
		b.WriteString("\n")
	case StmtBreak:
		if len(stmt.LoopID) > 0 {
			b.WriteString(stmt.LoopID[0].T)
			b.WriteString(" ")
		}
		b.WriteString("break\n")
	case StmtContinue:
		if len(stmt.LoopID) > 0 {
			b.WriteString(stmt.LoopID[0].T)
			b.WriteString(" ")
		}
		b.WriteString("continue\n")
	case StmtLoop:
		if len(stmt.LoopID) > 0 {
			b.WriteString(stmt.LoopID[0].T)
			b.WriteString(" ")
		}
		b.WriteString("{\n")
		for _, s := range stmt.Stmts {
			s.unparse(b, indent+addIndent)
		}
		b.WriteString(indent)
		b.WriteString("}\n")
	case StmtMessage:
		if len(stmt.LoopID) > 0 {
			b.WriteString(stmt.LoopID[0].T)
			b.WriteString(" ")
		}
		b.WriteString("[\n")
		for _, s := range stmt.Arms {
			s.unparse(b, indent+addIndent)
		}
		b.WriteString(indent)
		b.WriteString("]\n")
	case StmtSpawn:
		b.WriteString(stmt.Vars[0].T)
		sep := " < ["
		for _, expr := range stmt.Exprs {
			b.WriteString(sep)
			b.WriteString(expr.T)
			sep = " "
		}
		b.WriteString("]\n")
	}
}

func (guard Guard) unparse(b *strings.Builder) {
	if len(guard.Exprs) > 1 {
		b.WriteString(guard.Exprs[0].T)
	}
	if guard.Equal {
		b.WriteString("=")
	} else {
		b.WriteString("!")
	}
	if len(guard.Exprs) == 1 {
		b.WriteString(guard.Exprs[0].T)
	} else {
		b.WriteString(guard.Exprs[1].T)
	}
	b.WriteString(" ")
}

func (arm *Arm) unparse(b *strings.Builder, indent string) {
	b.WriteString(indent)
	for _, g := range arm.Guards {
		g.unparse(b)
	}
	switch arm.Type {
	case ArmRecv:
		b.WriteString(arm.Vars[0].T)
		b.WriteString(" ")
		b.WriteString(arm.Vars[1].T)
		b.WriteString(" <")
		for _, expr := range arm.Exprs {
			b.WriteString(" ")
			b.WriteString(expr.T)
		}
	case ArmSend:
		b.WriteString(arm.Exprs[0].T)
		b.WriteString(" < ")
		b.WriteString(arm.Exprs[1].T)
	}
	b.WriteString(" {\n")
	for _, s := range arm.Stmts {
		s.unparse(b, indent+addIndent)
	}
	b.WriteString(indent)
	b.WriteString("}\n")
}
