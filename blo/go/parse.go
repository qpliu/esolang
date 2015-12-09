package main

import (
	"errors"
)

type tokenStream struct {
	tokens  <-chan Token
	pending Token
}

func (t *tokenStream) next() Token {
	if t.pending.Token != "" {
		token := t.pending
		t.pending.Token = ""
		return token
	}
	if token, ok := <-t.tokens; ok {
		return token
	}
	return Token{Token: ""}
}

func (t *tokenStream) peek() Token {
	if t.pending.Token == "" {
		t.pending = t.next()
	}
	return t.pending
}

func (t *tokenStream) skipNewlines() {
	for t.peek().Token == "\n" {
		t.next()
	}
}

func Parse(tokens <-chan Token) (*Ast, error) {
	tokenStream := &tokenStream{tokens: tokens}
	ast := newAst()
	for {
		tokenStream.skipNewlines()
		switch tok := tokenStream.peek(); tok.Token {
		case "":
			return ast, nil
		case "import":
			if err := parseImport(tokenStream, ast); err != nil {
				return ast, err
			}
		case "type":
			if _, err := parseType(tokenStream, ast); err != nil {
				return ast, err
			}
		case "func":
			if funcDecl, err := parseFunc(tokenStream, ast); err != nil {
				return ast, err
			} else if stmts, err := parseStatementBlock(tokenStream); err != nil {
				return ast, err
			} else {
				funcDecl.Body = stmts
			}
		default:
			return ast, errors.New(tok.Location.String() + ": Expected 'import', 'type', or 'func', got:" + tok.Token)
		}
	}
}

func parseImport(tokenStream *tokenStream, ast *Ast) error {
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok.Token != "import" {
		panic("Expected 'import', got:" + tok.Token)
	}
	tokenStream.skipNewlines()
	switch tok := tokenStream.peek(); tok.Token {
	case "type":
		if typeDecl, err := parseType(tokenStream, ast); err != nil {
			return err
		} else {
			typeDecl.Imported = true
		}
	case "func":
		if funcDecl, err := parseFunc(tokenStream, ast); err != nil {
			return err
		} else {
			funcDecl.Imported = true
		}
		switch tok := tokenStream.peek(); tok.Token {
		case ";", "\n":
			tokenStream.next()
		default:
			return errors.New(tok.Location.String() + ": Expected ';', got:" + tok.Token)
		}
	default:
		return errors.New(tok.Location.String() + ": Expected 'type' or 'func', got:" + tok.Token)
	}
	return nil
}

func parseType(tokenStream *tokenStream, ast *Ast) (*Type, error) {
	tokenStream.skipNewlines()
	typeDecl := &Type{}
	if tok := tokenStream.next(); tok.Token != "type" {
		panic("Expected 'type', got:" + tok.Token)
	} else {
		typeDecl.Location = tok.Location
	}
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); !tok.IsIdentifier() {
		return nil, errors.New(tok.Location.String() + ": Expected type name identifier, got:" + tok.Token)
	} else {
		typeDecl.Name = tok.Token
	}
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok.Token != "{" {
		return nil, errors.New(tok.Location.String() + ": Expected '{', got:" + tok.Token)
	}
	if _, ok := ast.Types[typeDecl.Name]; ok {
		return nil, errors.New(typeDecl.Location.String() + ": Duplicate type name:" + typeDecl.Name)
	}
	for {
		tokenStream.skipNewlines()
		tok := tokenStream.peek()
		if tok.Token == "}" {
			tokenStream.next()
			break
		} else if !tok.IsIdentifier() {
			return nil, errors.New(tok.Location.String() + ": Expected field name identifier, got:" + tok.Token)
		}
		startIndex := len(typeDecl.Fields)
		for {
			tok = tokenStream.next()
			if !tok.IsIdentifier() {
				return nil, errors.New(tok.Location.String() + ": Expected field name identifier, got:" + tok.Token)
			}
			typeDecl.Fields = append(typeDecl.Fields, &Var{Name: tok.Token})
			tok = tokenStream.peek()
			if tok.IsIdentifier() {
				for i := startIndex; i < len(typeDecl.Fields); i++ {
					typeDecl.Fields[i].TypeName = tok.Token
				}
				tokenStream.next()
				break
			} else if tok.Token == ";" || tok.Token == "\n" || tok.Token == "}" {
				break
			} else if tok.Token == "," {
				tokenStream.next()
			} else {
				return nil, errors.New(tok.Location.String() + ": Expected field type identifier, ';', or ',', got:" + tok.Token)
			}
		}
		tok = tokenStream.peek()
		if tok.Token != ";" && tok.Token != "\n" && tok.Token != "}" {
			return nil, errors.New(tok.Location.String() + ": Expected ';' or '}', got:" + tok.Token)
		}
		if tok.Token == ";" {
			tokenStream.next()
		}
		tokenStream.skipNewlines()
	}
	names := make(map[string]bool)
	for _, field := range typeDecl.Fields {
		if _, ok := names[field.Name]; ok {
			return nil, errors.New(typeDecl.Location.String() + ": Duplicate field name:" + field.Name + " for type:" + typeDecl.Name)
		}
		names[field.Name] = true
	}
	ast.Types[typeDecl.Name] = typeDecl
	return typeDecl, nil
}

func parseFunc(tokenStream *tokenStream, ast *Ast) (*Func, error) {
	tokenStream.skipNewlines()
	funcDecl := &Func{}
	if tok := tokenStream.next(); tok.Token != "func" {
		panic("Expected 'func', got:" + tok.Token)
	} else {
		funcDecl.Location = tok.Location
	}
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); !tok.IsIdentifier() {
		return nil, errors.New(tok.Location.String() + ": Expected function name identifier, got:" + tok.Token)
	} else {
		funcDecl.Name = tok.Token
	}
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok.Token != "(" {
		return nil, errors.New(tok.Location.String() + ": Expected '(', got:" + tok.Token)
	}

	if _, ok := ast.Funcs[funcDecl.Name]; ok {
		return nil, errors.New(funcDecl.Location.String() + ": Duplicate func name:" + funcDecl.Name)
	}
	tokenStream.skipNewlines()
	if tok := tokenStream.peek(); tok.Token == ")" {
		tokenStream.next()
	} else {
		for {
			startIndex := len(funcDecl.Params)
			for {
				tokenStream.skipNewlines()
				tok := tokenStream.next()
				if !tok.IsIdentifier() {
					return nil, errors.New(tok.Location.String() + ": Expected parameter name identifier, got:" + tok.Token)
				}
				funcDecl.Params = append(funcDecl.Params, &Var{Name: tok.Token})
				tokenStream.skipNewlines()
				tok = tokenStream.next()
				if tok.Token == "," {
					continue
				} else if !tok.IsIdentifier() {
					return nil, errors.New(tok.Location.String() + ": Expected parameter type name identifier or ',', got:" + tok.Token)
				}
				for i := startIndex; i < len(funcDecl.Params); i++ {
					funcDecl.Params[i].TypeName = tok.Token
				}
				break
			}
			tok = tokenStream.next()
			if tok.Token == ")" {
				break
			} else if tok.Token != "," {
				return nil, errors.New(tok.Location.String() + ": Expected ',' or ')', got:" + tok.Token)
			}
		}
	}
	if tok := tokenStream.peek(); tok.IsIdentifier() {
		tokenStream.next()
		funcDecl.TypeName = tok.Token
	}
	names := make(map[string]bool)
	for _, param := range funcDecl.Params {
		if _, ok := names[param.Name]; ok {
			return nil, errors.New(funcDecl.Location.String() + ": Duplicate func parameter name:" + param.Name + " for func:" + funcDecl.Name)
		}
		names[param.Name] = true
	}
	ast.Funcs[funcDecl.Name] = funcDecl
	return funcDecl, nil
}

func parseStatementBlock(tokenStream *tokenStream) (*StmtBlock, error) {
	tokenStream.skipNewlines()
	stmtBlock := &StmtBlock{}
	if tok := tokenStream.next(); tok.Token != "{" {
		return nil, errors.New(tok.Location.String() + ": Expected '{', got:" + tok.Token)
	} else {
		stmtBlock.location = tok.Location
	}
	for {
		tokenStream.skipNewlines()
		tok := tokenStream.peek()
		switch tok.Token {
		case "}":
			tokenStream.next()
			return stmtBlock, nil
		case "{":
			if stmt, err := parseStatementBlock(tokenStream); err != nil {
				return nil, err
			} else {
				stmtBlock.Stmts = append(stmtBlock.Stmts, stmt)
			}
		case "var":
			if stmt, err := parseStmtVar(tokenStream); err != nil {
				return nil, err
			} else {
				stmtBlock.Stmts = append(stmtBlock.Stmts, stmt)
			}
		case "if":
			if stmt, err := parseStmtIf(tokenStream); err != nil {
				return nil, err
			} else {
				stmtBlock.Stmts = append(stmtBlock.Stmts, stmt)
			}
		case "for":
			if stmt, err := parseStmtFor(tokenStream); err != nil {
				return nil, err
			} else {
				stmtBlock.Stmts = append(stmtBlock.Stmts, stmt)
			}
		case "break":
			if stmt, err := parseStmtBreak(tokenStream); err != nil {
				return nil, err
			} else {
				stmtBlock.Stmts = append(stmtBlock.Stmts, stmt)
			}
		case "return":
			if stmt, err := parseStmtReturn(tokenStream); err != nil {
				return nil, err
			} else {
				stmtBlock.Stmts = append(stmtBlock.Stmts, stmt)
			}
		case "set", "clear":
			if stmt, err := parseStmtSetClear(tokenStream); err != nil {
				return nil, err
			} else {
				stmtBlock.Stmts = append(stmtBlock.Stmts, stmt)
			}
		default:
			if !tok.IsIdentifier() {
				return nil, errors.New(tok.Location.String() + ": Expected '}', 'var', 'if', 'for', 'break', 'return', 'set', 'clear', or expression, got:" + tok.Token)
			}
			if stmt, err := parseStmtExprOrAssign(tokenStream); err != nil {
				return nil, err
			} else {
				stmtBlock.Stmts = append(stmtBlock.Stmts, stmt)
			}
		}
	}
}

func parseStmtVar(tokenStream *tokenStream) (*StmtVar, error) {
	tokenStream.skipNewlines()
	stmt := &StmtVar{}
	if tok := tokenStream.next(); tok.Token != "var" {
		panic("Expected 'var', got:" + tok.Token)
	} else {
		stmt.location = tok.Location
	}
	if tok := tokenStream.next(); !tok.IsIdentifier() {
		return nil, errors.New(tok.Location.String() + ": Expected variable name identifier, got:" + tok.Token)
	} else {
		stmt.Name = tok.Token
	}
	if tok := tokenStream.next(); !tok.IsIdentifier() {
		return nil, errors.New(tok.Location.String() + ": Expected type name identifier, got:" + tok.Token)
	} else {
		stmt.TypeName = tok.Token
	}
	switch tok := tokenStream.peek(); tok.Token {
	case "=":
		tokenStream.next()
		if expr, err := parseExpr(tokenStream, false); err != nil {
			return nil, err
		} else {
			stmt.Expr = expr
		}
	case ";", "\n":
	default:
		return nil, errors.New(tok.Location.String() + ": Expected ';' or '=', got:" + tok.Token)
	}
	switch tok := tokenStream.peek(); tok.Token {
	case ";", "\n":
		tokenStream.next()
	default:
		return nil, errors.New(tok.Location.String() + ": Expected ';', got:" + tok.Token)
	}
	return stmt, nil
}

func parseStmtIf(tokenStream *tokenStream) (*StmtIf, error) {
	tokenStream.skipNewlines()
	stmt := &StmtIf{}
	if tok := tokenStream.next(); tok.Token != "if" {
		panic("Expected 'if', got:" + tok.Token)
	} else {
		stmt.location = tok.Location
	}
	if expr, err := parseExpr(tokenStream, true); err != nil {
		return nil, err
	} else {
		stmt.Expr = expr
	}
	if stmts, err := parseStatementBlock(tokenStream); err != nil {
		return nil, err
	} else {
		stmt.Stmts = stmts
	}
	tokenStream.skipNewlines()
	if tokenStream.peek().Token != "else" {
		return stmt, nil
	}
	tokenStream.next()
	tokenStream.skipNewlines()
	switch tok := tokenStream.peek(); tok.Token {
	case "if":
		if ifStmt, err := parseStmtIf(tokenStream); err != nil {
			return nil, err
		} else {
			stmt.ElseIf = ifStmt
		}
	case "{":
		if stmts, err := parseStatementBlock(tokenStream); err != nil {
			return nil, err
		} else {
			stmt.Else = stmts
		}
	default:
		return nil, errors.New(tok.Location.String() + ": Expected '{' or 'if', got:" + tok.Token)
	}
	return stmt, nil
}

func parseStmtFor(tokenStream *tokenStream) (*StmtFor, error) {
	tokenStream.skipNewlines()
	stmt := &StmtFor{}
	if tok := tokenStream.next(); tok.Token != "for" {
		panic("Expected 'for', got:" + tok.Token)
	} else {
		stmt.location = tok.Location
	}
	tokenStream.skipNewlines()
	if tok := tokenStream.peek(); tok.IsIdentifier() {
		tokenStream.next()
		stmt.Label = tok.Token
	}
	if stmts, err := parseStatementBlock(tokenStream); err != nil {
		return nil, err
	} else {
		stmt.Stmts = stmts
	}
	return stmt, nil
}

func parseStmtBreak(tokenStream *tokenStream) (*StmtBreak, error) {
	tokenStream.skipNewlines()
	stmt := &StmtBreak{}
	if tok := tokenStream.next(); tok.Token != "break" {
		panic("Expected 'break', got:" + tok.Token)
	} else {
		stmt.location = tok.Location
	}
	if tok := tokenStream.peek(); tok.IsIdentifier() {
		tokenStream.next()
		stmt.Label = tok.Token
	}
	switch tok := tokenStream.peek(); tok.Token {
	case ";", "\n":
		tokenStream.next()
	default:
		return nil, errors.New(tok.Location.String() + ": Expected ';', got:" + tok.Token)
	}
	return stmt, nil
}

func parseStmtReturn(tokenStream *tokenStream) (*StmtReturn, error) {
	tokenStream.skipNewlines()
	stmt := &StmtReturn{}
	if tok := tokenStream.next(); tok.Token != "return" {
		panic("Expected 'return', got:" + tok.Token)
	} else {
		stmt.location = tok.Location
	}
	switch tok := tokenStream.peek(); tok.Token {
	case ";", "\n":
	default:
		if tok.IsIdentifier() {
			if expr, err := parseExpr(tokenStream, false); err != nil {
				return nil, err
			} else {
				stmt.Expr = expr
			}
		} else {
			return nil, errors.New(tok.Location.String() + ": Expected ';' or expression, got:" + tok.Token)
		}
	}
	switch tok := tokenStream.peek(); tok.Token {
	case ";", "\n":
		tokenStream.next()
	default:
		return nil, errors.New(tok.Location.String() + ": Expected ';', got:" + tok.Token)
	}
	return stmt, nil
}

func parseStmtSetClear(tokenStream *tokenStream) (*StmtSetClear, error) {
	tokenStream.skipNewlines()
	stmtTok := tokenStream.next()
	if stmtTok.Token != "set" && stmtTok.Token != "clear" {
		panic("Expected 'set' or 'clear', got:" + stmtTok.Token)
	}
	tokenStream.skipNewlines()
	stmt := &StmtSetClear{location: stmtTok.Location, Value: stmtTok.Token == "set"}
	if expr, err := parseExpr(tokenStream, false); err != nil {
		return nil, err
	} else {
		stmt.Expr = expr
	}
	switch tok := tokenStream.peek(); tok.Token {
	case ";", "\n":
		tokenStream.next()
	default:
		return nil, errors.New(tok.Location.String() + ": Expected ';', got:" + tok.Token)
	}
	return stmt, nil
}

func parseStmtExprOrAssign(tokenStream *tokenStream) (Stmt, error) {
	tokenStream.skipNewlines()
	var lvalue Expr
	if expr, err := parseExpr(tokenStream, false); err != nil {
		return nil, err
	} else {
		lvalue = expr
	}
	switch tok := tokenStream.peek(); tok.Token {
	case ";", "\n":
		tokenStream.next()
		return &StmtExpr{Expr: lvalue}, nil
	case "=":
		tokenStream.next()
	default:
		return nil, errors.New(tok.Location.String() + ": Expected '=' or ';', got:" + tok.Token)
	}
	stmt := &StmtAssign{LValue: lvalue}
	tokenStream.skipNewlines()
	if expr, err := parseExpr(tokenStream, false); err != nil {
		return nil, err
	} else {
		stmt.Expr = expr
	}
	switch tok := tokenStream.peek(); tok.Token {
	case ";", "\n":
		tokenStream.next()
	default:
		return nil, errors.New(tok.Location.String() + ": Expected ';', got:" + tok.Token)
	}
	return stmt, nil
}

func parseExpr(tokenStream *tokenStream, ignoreNewlines bool) (Expr, error) {
	name := tokenStream.next()
	if !name.IsIdentifier() {
		return nil, errors.New(name.Location.String() + ": Expected function name identifier or local variable identifier, got:" + name.Token)
	}
	if ignoreNewlines {
		tokenStream.skipNewlines()
	}
	switch tok := tokenStream.peek(); tok.Token {
	case ".":
		return parseExprField(tokenStream, ignoreNewlines, &ExprVar{location: name.Location, Name: name.Token})
	case "(":
		if expr, err := parseExprFuncParams(tokenStream, name); err != nil {
			return nil, err
		} else {
			return parseExprField(tokenStream, ignoreNewlines, expr)
		}
	default:
		return &ExprVar{location: name.Location, Name: name.Token}, nil
	}
}

func parseExprField(tokenStream *tokenStream, ignoreNewlines bool, expr Expr) (Expr, error) {
	for {
		if ignoreNewlines {
			tokenStream.skipNewlines()
		}
		switch tok := tokenStream.peek(); tok.Token {
		case ".":
			tokenStream.next()
			tokenStream.skipNewlines()
			fieldName := tokenStream.next()
			if !fieldName.IsIdentifier() {
				return nil, errors.New(fieldName.Location.String() + ": Expected field name identifier, got:" + fieldName.Token)
			}
			expr = &ExprField{Name: fieldName.Token, Expr: expr}
		default:
			return expr, nil
		}
	}
}

func parseExprFuncParams(tokenStream *tokenStream, name Token) (*ExprFunc, error) {
	if tok := tokenStream.next(); tok.Token != "(" {
		panic("Expected '(', got:" + tok.Token)
	}
	tokenStream.skipNewlines()
	expr := &ExprFunc{location: name.Location, Name: name.Token}
	if tokenStream.peek().Token == ")" {
		tokenStream.next()
		return expr, nil
	}
	for {
		tokenStream.skipNewlines()
		if param, err := parseExpr(tokenStream, true); err != nil {
			return nil, err
		} else {
			expr.Params = append(expr.Params, param)
		}
		tokenStream.skipNewlines()
		switch tok := tokenStream.next(); tok.Token {
		case ")":
			return expr, nil
		case ",":
		default:
			return nil, errors.New(tok.Location.String() + ": Expected ')' or ',', got:" + tok.Token)
		}
	}
}
