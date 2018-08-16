package main

import (
	"bytes"
	"fmt"
	"io"
)

type Statement struct {
	Tokens []*Token

	Label   uint16
	Percent uint16
	Not     bool

	Type     StatementType
	Operands interface{}
}

type StatementType int

const (
	StatementUnrecognizable StatementType = iota
	StatementCalculate16
	StatementCalculate32
	StatementCalculate16ArrayElement
	StatementCalculate32ArrayElement
	StatementCalculate16ArrayDimension
	StatementCalculate32ArrayDimension
	StatementNext
	StatementForget
	StatementResume
	StatementStash
	StatementRetrieve
	StatementIgnore
	StatementRemember
	StatementAbstainLabel
	StatementAbstainGerundList
	StatementReinstateLabel
	StatementReinstateGerundList
	StatementGiveUp
	StatementWriteIn
	StatementReadOut
)

const (
	mkStmtStateOther = iota
	mkStmtStateWax
	mkStmtStateWaxNumber
	mkStmtStateWaxNumberWane
	mkStmtStatePlease
)

func Parse(t *Tokenizer) ([]*Statement, error) {
	statements := []*Statement{}
	tokens := []*Token{}
	state := mkStmtStateOther
	for {
		token, err := t.Next()
		if err != nil {
			if err != io.EOF {
				return nil, err
			} else {
				if len(tokens) > 0 {
					statements = append(statements, &Statement{Tokens: tokens})
				}
				for _, s := range statements {
					if err := s.Parse(); err != nil {
						return nil, err
					}
				}
				return statements, nil
			}
		}
		switch token.Type {
		case TokenDo:
			if state == mkStmtStatePlease {
				tokens = append(tokens, token)
				state = mkStmtStateOther
				break
			}
			fallthrough
		case TokenPlease:
			if state == mkStmtStateWaxNumberWane && len(tokens) > 3 {
				newTokens := []*Token{tokens[len(tokens)-3], tokens[len(tokens)-2], tokens[len(tokens)-1], token}
				statements = append(statements, &Statement{Tokens: tokens[:len(tokens)-3]})
				tokens = newTokens
			} else {
				if len(tokens) > 0 {
					statements = append(statements, &Statement{Tokens: tokens})
				}
				tokens = []*Token{token}
			}
			if token.Type == TokenPlease {
				state = mkStmtStatePlease
			} else {
				state = mkStmtStateOther
			}
		case TokenWax:
			tokens = append(tokens, token)
			state = mkStmtStateWax
		case TokenNumber:
			tokens = append(tokens, token)
			if state == mkStmtStateWax {
				state = mkStmtStateWaxNumber
			} else {
				state = mkStmtStateOther
			}
		case TokenWane:
			tokens = append(tokens, token)
			if state == mkStmtStateWaxNumber {
				state = mkStmtStateWaxNumberWane
			} else {
				state = mkStmtStateOther
			}
		default:
			tokens = append(tokens, token)
			state = mkStmtStateOther
		}
	}
}

func (s *Statement) String() string {
	b := bytes.Buffer{}
	newLine := false
	for _, t := range s.Tokens {
		if newLine {
			b.WriteString("\n")
		}
		if len(t.StringValue) > 0 && t.StringValue[len(t.StringValue)-1] == '\n' {
			b.WriteString(t.StringValue[:len(t.StringValue)-1])
			newLine = true
		} else {
			b.WriteString(t.StringValue)
			newLine = false
		}
	}
	return b.String()
}

func (s *Statement) Parse() error {
	//...
	return nil
}

func ListStatements(statements []*Statement, out io.Writer) error {
	for i, s := range statements {
		c := ' '
		if s.Type == StatementUnrecognizable {
			c = '*'
		}
		_, err := fmt.Fprintf(out, "%c%4d %s\n", c, i+1, s.String())
		if err != nil {
			return err
		}
	}
	return nil
}
