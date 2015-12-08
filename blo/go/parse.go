package main

import (
	"errors"
)

type tokenStream struct {
	tokens  <-chan Token
	pending Token
}

func (t *tokenStream) next() Token {
	if t.pending != "" {
		token := t.pending
		t.pending = ""
		return token
	}
	if token, ok := <-t.tokens; ok {
		return token
	}
	return ""
}

func (t *tokenStream) peek() Token {
	if t.pending == "" {
		t.pending = t.next()
	}
	return t.pending
}

func (t *tokenStream) skipNewlines() {
	for t.peek() == "\n" {
		t.next()
	}
}

func Parse(tokens <-chan Token) (*Ast, error) {
	tokenStream := &tokenStream{tokens: tokens}
	ast := newAst()
	for {
		tokenStream.skipNewlines()
		switch tok := tokenStream.peek(); tok {
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
			return ast, errors.New("Expected 'import', 'type', or 'func', got:" + string(tok))
		}
	}
}

func parseImport(tokenStream *tokenStream, ast *Ast) error {
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok != "import" {
		panic("Expected 'import', got:" + string(tok))
	}
	tokenStream.skipNewlines()
	switch tok := tokenStream.peek(); tok {
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
		switch tok := tokenStream.peek(); tok {
		case ";", "\n":
			tokenStream.next()
		default:
			return errors.New("Expected ';', got:" + string(tok))
		}
	default:
		return errors.New("Expected 'type' or 'func', got:" + string(tok))
	}
	return nil
}

func parseType(tokenStream *tokenStream, ast *Ast) (*Type, error) {
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok != "type" {
		panic("Expected 'type', got:" + string(tok))
	}
	tokenStream.skipNewlines()
	name := tokenStream.next()
	if !name.IsIdentifier() {
		return nil, errors.New("Expected function name identifier, got:" + string(name))
	}
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok != "{" {
		return nil, errors.New("Expected '{', got:" + string(tok))
	}
	typeDecl := &Type{Name: string(name)}
	if _, ok := ast.Types[typeDecl.Name]; ok {
		return nil, errors.New("Duplicate type name:" + typeDecl.Name)
	}
	for {
		tokenStream.skipNewlines()
		tok := tokenStream.peek()
		if tok == "}" {
			tokenStream.next()
			break
		} else if !tok.IsIdentifier() {
			return nil, errors.New("Expected field name identifier, got:" + string(tok))
		}
		startIndex := len(typeDecl.Fields)
		for {
			tok = tokenStream.next()
			if !tok.IsIdentifier() {
				return nil, errors.New("Expected field name identifier, got:" + string(tok))
			}
			typeDecl.Fields = append(typeDecl.Fields, &Var{Name: string(tok)})
			tok = tokenStream.peek()
			if tok.IsIdentifier() {
				for i := startIndex; i < len(typeDecl.Fields); i++ {
					typeDecl.Fields[i].TypeName = string(tok)
				}
				tokenStream.next()
				break
			} else if tok == ";" || tok == "\n" {
				break
			} else if tok == "," {
				tokenStream.next()
			} else {
				return nil, errors.New("Expected field type identifier, ';', or ',', got:" + string(tok))
			}
		}
		tok = tokenStream.peek()
		if tok != ";" && tok != "\n" && tok != "}" {
			return nil, errors.New("Expected ';' or '}', got:" + string(tok))
		}
		if tok == ";" {
			tokenStream.next()
		}
		tokenStream.skipNewlines()
	}
	names := make(map[string]bool)
	for _, field := range typeDecl.Fields {
		if _, ok := names[field.Name]; ok {
			return nil, errors.New("Duplicate field name:" + field.Name + " for type:" + typeDecl.Name)
		}
		names[field.Name] = true
	}
	ast.Types[typeDecl.Name] = typeDecl
	return typeDecl, nil
}

func parseFunc(tokenStream *tokenStream, ast *Ast) (*Func, error) {
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok != "func" {
		panic("Expected 'func', got:" + string(tok))
	}
	tokenStream.skipNewlines()
	name := tokenStream.next()
	if !name.IsIdentifier() {
		return nil, errors.New("Expected function name identifier, got:" + string(name))
	}
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok != "(" {
		return nil, errors.New("Expected '(', got:" + string(tok))
	}
	funcDecl := &Func{Name: string(name)}
	if _, ok := ast.Funcs[funcDecl.Name]; ok {
		return nil, errors.New("Duplicate func name:" + funcDecl.Name)
	}
	tokenStream.skipNewlines()
	if tok := tokenStream.peek(); tok == ")" {
		tokenStream.next()
	} else {
		for {
			startIndex := len(funcDecl.Params)
			for {
				tokenStream.skipNewlines()
				tok := tokenStream.next()
				if !tok.IsIdentifier() {
					return nil, errors.New("Expected parameter name identifier, got:" + string(tok))
				}
				funcDecl.Params = append(funcDecl.Params, &Var{Name: string(tok)})
				tokenStream.skipNewlines()
				tok = tokenStream.next()
				if tok == "," {
					continue
				} else if !tok.IsIdentifier() {
					return nil, errors.New("Expected parameter type name identifier or ',', got:" + string(tok))
				}
				for i := startIndex; i < len(funcDecl.Params); i++ {
					funcDecl.Params[i].TypeName = string(tok)
				}
				break
			}
			tok = tokenStream.next()
			if tok == ")" {
				break
			} else if tok != "," {
				return nil, errors.New("Expected ',' or ')', got:" + string(tok))
			}
		}
	}
	if tok := tokenStream.peek(); tok.IsIdentifier() {
		tokenStream.next()
		funcDecl.TypeName = string(tok)
	}
	names := make(map[string]bool)
	for _, param := range funcDecl.Params {
		if _, ok := names[param.Name]; ok {
			return nil, errors.New("Duplicate func parameter name:" + param.Name + " for func:" + funcDecl.Name)
		}
		names[param.Name] = true
	}
	ast.Funcs[funcDecl.Name] = funcDecl
	return funcDecl, nil
}

func parseStatementBlock(tokenStream *tokenStream) (*StmtBlock, error) {
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok != "{" {
		return nil, errors.New("Expected '{', got:" + string(tok))
	}
	stmtBlock := &StmtBlock{}
	for {
		tokenStream.skipNewlines()
		tok := tokenStream.peek()
		switch tok {
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
				return nil, errors.New("Expected '}', 'var', 'if', 'for', 'break', 'return', 'set', 'clear', or expression, got:" + string(tok))
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
	if tok := tokenStream.next(); tok != "var" {
		panic("Expected 'var', got:" + string(tok))
	}
	name := tokenStream.next()
	if !name.IsIdentifier() {
		return nil, errors.New("Expected variable name identifier, got:" + string(name))
	}
	typeName := tokenStream.next()
	if !typeName.IsIdentifier() {
		return nil, errors.New("Expected type name identifier, got:" + string(typeName))
	}
	stmt := &StmtVar{Name: string(name), TypeName: string(typeName)}
	switch tok := tokenStream.peek(); tok {
	case "=":
		tokenStream.next()
		if expr, err := parseExpr(tokenStream, false); err != nil {
			return nil, err
		} else {
			stmt.Expr = expr
		}
	case ";", "\n":
	default:
		return nil, errors.New("Expected ';' or '=', got:" + string(tok))
	}
	switch tok := tokenStream.peek(); tok {
	case ";", "\n":
		tokenStream.next()
	default:
		return nil, errors.New("Expected ';', got:" + string(tok))
	}
	return stmt, nil
}

func parseStmtIf(tokenStream *tokenStream) (*StmtIf, error) {
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok != "if" {
		panic("Expected 'if', got:" + string(tok))
	}
	stmt := &StmtIf{}
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
	if tokenStream.peek() != "else" {
		return stmt, nil
	}
	tokenStream.next()
	tokenStream.skipNewlines()
	switch tok := tokenStream.peek(); tok {
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
		return nil, errors.New("Expected '{' or 'if', got:" + string(tok))
	}
	return stmt, nil
}

func parseStmtFor(tokenStream *tokenStream) (*StmtFor, error) {
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok != "for" {
		panic("Expected 'for', got:" + string(tok))
	}
	tokenStream.skipNewlines()
	stmt := &StmtFor{}
	if tok := tokenStream.peek(); tok.IsIdentifier() {
		tokenStream.next()
		stmt.Label = string(tok)
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
	if tok := tokenStream.next(); tok != "break" {
		panic("Expected 'break', got:" + string(tok))
	}
	stmt := &StmtBreak{}
	if tok := tokenStream.peek(); tok.IsIdentifier() {
		tokenStream.next()
		stmt.Label = string(tok)
	}
	switch tok := tokenStream.peek(); tok {
	case ";", "\n":
		tokenStream.next()
	default:
		return nil, errors.New("Expected ';', got:" + string(tok))
	}
	return stmt, nil
}

func parseStmtReturn(tokenStream *tokenStream) (*StmtReturn, error) {
	tokenStream.skipNewlines()
	if tok := tokenStream.next(); tok != "return" {
		panic("Expected 'return', got:" + string(tok))
	}
	stmt := &StmtReturn{}
	switch tok := tokenStream.peek(); tok {
	case ";", "\n":
	default:
		if tok.IsIdentifier() {
			if expr, err := parseExpr(tokenStream, false); err != nil {
				return nil, err
			} else {
				stmt.Expr = expr
			}
		} else {
			return nil, errors.New("Expected ';' or expression, got:" + string(tok))
		}
	}
	switch tok := tokenStream.peek(); tok {
	case ";", "\n":
		tokenStream.next()
	default:
		return nil, errors.New("Expected ';', got:" + string(tok))
	}
	return stmt, nil
}

func parseStmtSetClear(tokenStream *tokenStream) (*StmtSetClear, error) {
	tokenStream.skipNewlines()
	stmtTok := tokenStream.next()
	if stmtTok != "set" && stmtTok != "clear" {
		panic("Expected 'set' or 'clear', got:" + string(stmtTok))
	}
	tokenStream.skipNewlines()
	stmt := &StmtSetClear{Value: stmtTok == "set"}
	if expr, err := parseExpr(tokenStream, false); err != nil {
		return nil, err
	} else {
		stmt.Expr = expr
	}
	switch tok := tokenStream.peek(); tok {
	case ";", "\n":
		tokenStream.next()
	default:
		return nil, errors.New("Expected ';', got:" + string(tok))
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
	switch tok := tokenStream.peek(); tok {
	case ";", "\n":
		tokenStream.next()
		return &StmtExpr{Expr: lvalue}, nil
	case "=":
		tokenStream.next()
	default:
		return nil, errors.New("Expected '=' or ';', got:" + string(tok))
	}
	stmt := &StmtAssign{LValue: lvalue}
	tokenStream.skipNewlines()
	if expr, err := parseExpr(tokenStream, false); err != nil {
		return nil, err
	} else {
		stmt.Expr = expr
	}
	switch tok := tokenStream.peek(); tok {
	case ";", "\n":
		tokenStream.next()
	default:
		return nil, errors.New("Expected ';', got:" + string(tok))
	}
	return stmt, nil
}

func parseExpr(tokenStream *tokenStream, ignoreNewlines bool) (Expr, error) {
	name := tokenStream.next()
	if !name.IsIdentifier() {
		return nil, errors.New("Expected function name identifier or local variable identifier, got:" + string(name))
	}
	if ignoreNewlines {
		tokenStream.skipNewlines()
	}
	switch tok := tokenStream.peek(); tok {
	case ".":
		return parseExprField(tokenStream, ignoreNewlines, &ExprVar{Name: string(name)})
	case "(":
		if expr, err := parseExprFuncParams(tokenStream, string(name)); err != nil {
			return nil, err
		} else {
			return parseExprField(tokenStream, ignoreNewlines, expr)
		}
	default:
		return &ExprVar{Name: string(name)}, nil
	}
}

func parseExprField(tokenStream *tokenStream, ignoreNewlines bool, expr Expr) (Expr, error) {
	for {
		if ignoreNewlines {
			tokenStream.skipNewlines()
		}
		switch tok := tokenStream.peek(); tok {
		case ".":
			tokenStream.next()
			tokenStream.skipNewlines()
			fieldName := tokenStream.next()
			if !fieldName.IsIdentifier() {
				return nil, errors.New("Expected field name identifier, got:" + string(fieldName))
			}
			expr = &ExprField{Name: string(fieldName), Expr: expr}
		default:
			return expr, nil
		}
	}
}

func parseExprFuncParams(tokenStream *tokenStream, name string) (*ExprFunc, error) {
	if tok := tokenStream.next(); tok != "(" {
		panic("Expected '(', got:" + string(tok))
	}
	tokenStream.skipNewlines()
	expr := &ExprFunc{Name: name}
	if tokenStream.peek() == ")" {
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
		switch tok := tokenStream.next(); tok {
		case ")":
			return expr, nil
		case ",":
		default:
			return nil, errors.New("Expected ')' or ',', got:" + string(tok))
		}
	}
}
