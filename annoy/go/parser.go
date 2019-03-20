package main

import (
	"fmt"
	"io"
)

func Parse(t *Tokenizer) ([]Stmt, error) {
	stmts := []Stmt{}
	tokens := []Token{}
	for {
		var stmt Stmt
		var err error
		stmt, tokens, err = parseStmt(t, tokens)
		if err == io.EOF {
			if len(stmts) == 0 {
				return stmts, fmt.Errorf("Unexpected EOF")
			}
			return stmts, nil
		} else if err != nil {
			return stmts, err
		}
		stmts = append(stmts, stmt)
		if len(tokens) == 0 {
			tokens, err = t.Append(tokens)
			if err == io.EOF {
				return stmts, nil
			} else if err != nil {
				return stmts, err
			}
		}
		if tokens[0].IsToken(".") {
			tokens = tokens[1:]
		} else {
			return nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
		}
	}
}

func parseStmt(t *Tokenizer, tokens []Token) (Stmt, []Token, error) {
	var err error
	if len(tokens) < 1 {
		tokens, err = t.Append(tokens)
		if err != nil {
			return nil, nil, err
		}
	}
	if tokens[0].IsToken("{") {
		var stmtBlock *StmtBlock
		stmtBlock, tokens, err = parseBlock(t, tokens[1:])
		return stmtBlock, tokens, err
	} else if tokens[0].IsToken("(") {
		var expr Expr
		expr, tokens, err = parseExpr(t, tokens)
		if err != nil {
			return nil, nil, err
		}
		return &StmtExpr{Expr: expr}, tokens, nil
	} else if !tokens[0].Identifier {
		return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
	}
	if len(tokens) < 2 {
		tokens, err = t.Append(tokens)
		if err != nil {
			return nil, nil, err
		}
	}
	if tokens[1].Identifier {
		return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[1].Filename, tokens[1].Line, tokens[1].Column)
	} else if tokens[1].IsToken(":=") {
		// assignment
		name := tokens[0]
		var expr Expr
		expr, tokens, err = parseExpr(t, tokens[2:])
		if err != nil {
			return nil, nil, err
		}
		return &StmtAssignment{Name: name, Expr: expr}, tokens, nil
	} else if tokens[1].IsToken("(") {
		// possibly define-function, but could be call-function, look for the :=
		var closeIndex int
		closeIndex, tokens, err = matchingCloseParen(t, tokens, 1)
		if err != nil {
			return nil, nil, err
		}
		if len(tokens) <= closeIndex+1 {
			tokens, err = t.Append(tokens)
			if err != nil && err != io.EOF {
				return nil, nil, err
			}
		}
		if len(tokens) > closeIndex+1 && tokens[closeIndex+1].IsToken(":=") {
			return parseDefineFunction(t, tokens, closeIndex)
		}
		// call-function, fall through
	}
	var expr Expr
	expr, tokens, err = parseExpr(t, tokens)
	if err != nil {
		return nil, nil, err
	}
	return &StmtExpr{Expr: expr}, tokens, nil
}

func parseBlock(t *Tokenizer, tokens []Token) (*StmtBlock, []Token, error) {
	stmts := []Stmt{}
	for {
		var err error
		if len(tokens) == 0 {
			tokens, err = t.Append(tokens)
			if err != nil {
				return nil, nil, err
			}
		}
		if tokens[0].IsToken("}") {
			if len(stmts) == 0 {
				return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
			}
			if stmtExpr, ok := stmts[len(stmts)-1].(*StmtExpr); !ok {
				return nil, nil, fmt.Errorf("%s:%d:%d: Invalid block", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
			} else {
				return &StmtBlock{
					Stmts:  stmts[:len(stmts)-1],
					Expr:   stmtExpr.Expr,
					Return: false,
				}, tokens[1:], nil
			}
		} else if tokens[0].IsToken("return") {
			var expr Expr
			expr, tokens, err = parseExpr(t, tokens[1:])
			if err != nil {
				return nil, nil, err
			}
			if len(tokens) == 0 {
				tokens, err = t.Append(tokens)
				if err != nil {
					return nil, nil, err
				}
			}
			if !tokens[0].IsToken("}") {
				return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
			}
			return &StmtBlock{
				Stmts:  stmts,
				Expr:   expr,
				Return: true,
			}, tokens[1:], nil
		}
		var stmt Stmt
		stmt, tokens, err = parseStmt(t, tokens)
		if err != nil {
			return nil, nil, err
		}
		stmts = append(stmts, stmt)
		if len(tokens) == 0 {
			tokens, err = t.Append(tokens)
			if err != nil {
				return nil, nil, err
			}
		}
		if tokens[0].IsToken(".") {
			tokens = tokens[1:]
			if len(tokens) == 0 {
				tokens, err = t.Append(tokens)
				if err != nil {
					return nil, nil, err
				}
			}
			if tokens[0].IsToken("}") {
				return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
			}
		} else if !tokens[0].IsToken("}") {
			return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
		}
	}
}

func parseExpr(t *Tokenizer, tokens []Token) (Expr, []Token, error) {
	var err error
	if len(tokens) == 0 {
		tokens, err = t.Append(tokens)
		if err != nil {
			return nil, nil, err
		}
	}
	var expr Expr
	if tokens[0].IsToken("(") {
		expr, tokens, err = parseExpr(t, tokens[1:])
		if err != nil {
			return nil, nil, err
		}
		if len(tokens) == 0 {
			tokens, err = t.Append(tokens)
			if err != nil {
				return nil, nil, err
			}
		}
		if !tokens[0].IsToken("(") {
			return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
		}
		tokens = tokens[1:]
	} else if tokens[0].IsToken("0") {
		expr = &Expr0{Token: tokens[0]}
		tokens = tokens[1:]
	} else if !tokens[0].Identifier {
		return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
	} else {
		// identifier or function-call
		for len(tokens) < 2 {
			tokens, err = t.Append(tokens)
			if err == io.EOF {
				return expr, tokens, nil
			} else if err != nil {
				return nil, nil, err
			}
		}
		if !tokens[1].IsToken("(") {
			expr = &ExprIdentifier{Name: tokens[0]}
			tokens = tokens[1:]
		} else {
			// function-call
			name := tokens[0]
			args := []Expr{}
			tokens = tokens[2:]
			for {
				if len(tokens) == 0 {
					tokens, err = t.Append(tokens)
					if err != nil {
						return nil, nil, err
					}
				}
				if tokens[0].IsToken(")") {
					tokens = tokens[1:]
					break
				}
				var arg Expr
				arg, tokens, err = parseExpr(t, tokens)
				if err != nil {
					return nil, nil, err
				}
				args = append(args, arg)
				if len(tokens) == 0 {
					tokens, err = t.Append(tokens)
					if err != nil {
						return nil, nil, err
					}
				}
				if tokens[0].IsToken(")") {
					tokens = tokens[1:]
					break
				} else if tokens[0].IsToken(",") {
					tokens = tokens[1:]
				} else {
					return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
				}
			}
			return &ExprCallFunction{Name: name, Args: args}, tokens, nil
		}
	}

	if len(tokens) == 0 {
		tokens, err = t.Append(tokens)
		if err == io.EOF {
			return expr, tokens, nil
		} else if err != nil {
			return nil, nil, err
		}
	}
	if tokens[0].IsToken("+") || tokens[0].IsToken(">") || tokens[0].IsToken("<") || tokens[0].IsToken("=") {
		op := tokens[0]
		var expr2 Expr
		expr2, tokens, err = parseExpr(t, tokens[1:])
		if err != nil {
			return nil, nil, err
		}
		if len(tokens) == 0 {
			tokens, err = t.Append(tokens)
			if err == io.EOF {
				return &ExprBinary{
					Left:  expr,
					Right: expr2,
					Op:    op,
					Block: nil,
				}, tokens, nil
			} else if err != nil {
				return nil, nil, err
			}
		}
		if !tokens[0].IsToken("{") {
			return &ExprBinary{
				Left:  expr,
				Right: expr2,
				Op:    op,
				Block: nil,
			}, tokens, nil
		}
		var stmtBlock *StmtBlock
		stmtBlock, tokens, err = parseBlock(t, tokens[1:])
		if err != nil {
			return nil, nil, err
		}
		return &ExprBinary{
			Left:  expr,
			Right: expr2,
			Op:    op,
			Block: stmtBlock,
		}, tokens, nil
	} else if tokens[0].IsToken("-") {
		tokens = tokens[1:]
		if len(tokens) == 0 {
			tokens, err = t.Append(tokens)
			if err == io.EOF {
				return &ExprPop{Expr: expr, Block: nil}, tokens, nil
			} else if err != nil {
				return nil, nil, err
			}
		}
		if !tokens[0].Identifier {
			return &ExprPop{Expr: expr, Block: nil}, tokens, nil
		}
		name := tokens[0]
		tokens = tokens[1:]
		if len(tokens) == 0 {
			tokens, err = t.Append(tokens)
			if err != nil {
				return nil, nil, err
			}
		}
		if !tokens[0].IsToken("{") {
			return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
		}
		tokens = tokens[1:]
		var stmtBlock *StmtBlock
		stmtBlock, tokens, err = parseBlock(t, tokens[1:])
		if err != nil {
			return nil, nil, err
		}
		return &ExprPop{
			Expr: expr,
			Block: &struct {
				Name  Token
				Block StmtBlock
			}{
				Name:  name,
				Block: *stmtBlock,
			},
		}, tokens, nil
	} else {
		return expr, tokens, nil
	}
}

func matchingCloseParen(t *Tokenizer, tokens []Token, openIndex int) (int, []Token, error) {
	level := 0
	for i := openIndex + 1; i < len(tokens); i++ {
		if tokens[i].IsToken("(") {
			level++
		} else if tokens[i].IsToken(")") {
			if level == 0 {
				return i, tokens, nil
			}
			level--
		}
	}
	for {
		var err error
		tokens, err = t.Append(tokens)
		if err != nil {
			return 0, nil, err
		}
		i := len(tokens) - 1
		if tokens[i].IsToken("(") {
			level++
		} else if tokens[i].IsToken(")") {
			if level == 0 {
				return i, tokens, nil
			}
			level--
		}
	}
}

func parseDefineFunction(t *Tokenizer, tokens []Token, closeIndex int) (Stmt, []Token, error) {
	if closeIndex+1 >= len(tokens) || closeIndex < 2 || len(tokens) < 4 || !tokens[0].Identifier || !tokens[1].IsToken("(") || !tokens[closeIndex].IsToken(")") || !tokens[closeIndex+1].IsToken(":=") {
		panic("parseDefineFunction")
	}
	name := tokens[0]
	params := []Token{}
	paramSet := make(map[string]bool)
	for i := 2; i < closeIndex; i += 2 {
		if i > 2 && tokens[i-1].IsToken(",") {
			return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[i-1].Filename, tokens[i-1].Line, tokens[i-1].Column)
		}
		if !tokens[i].Identifier {
			return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[i].Filename, tokens[i].Line, tokens[i].Column)
		}
		if paramSet[tokens[i].Value] {
			return nil, nil, fmt.Errorf("%s:%d:%d: Duplicate parameter name", tokens[i].Filename, tokens[i].Line, tokens[i].Column)
		}
		params = append(params, tokens[i])
		paramSet[tokens[i].Value] = true
	}
	tokens = tokens[closeIndex+1:]
	if len(tokens) == 0 {
		var err error
		tokens, err = t.Append(tokens)
		if err != nil {
			return nil, nil, err
		}
	}
	if tokens[0].Identifier {
		return &StmtDefineLibFunc{
			Name:   name,
			Params: params,
			Lib:    tokens[0],
		}, tokens[1:], nil
	} else if !tokens[0].IsToken("{") {
		return nil, nil, fmt.Errorf("%s:%d:%d: Unexpected token", tokens[0].Filename, tokens[0].Line, tokens[0].Column)
	}
	var stmtBlock *StmtBlock
	var err error
	stmtBlock, tokens, err = parseBlock(t, tokens[1:])
	if err != nil {
		return nil, nil, err
	}
	return &StmtDefineFunc{
		Name:   name,
		Params: params,
		Body:   *stmtBlock,
	}, tokens, nil
}
