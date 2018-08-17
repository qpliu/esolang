package main

import (
	"bytes"
	"fmt"
	"io"
)

type Statement struct {
	Tokens []*Token

	Label  uint16
	Chance uint16
	Not    bool

	Type     StatementType
	Operands interface{}
	Index    int
	Error    *Error
}

type StatementType int

const (
	StatementUnrecognizable StatementType = iota

	StatementCalculate
	StatementCalculateArrayDimension
	// Operands is Calculation

	StatementNext
	// Operands is initially uint16, the label
	// Resolve() changes it to int, the statement index

	StatementForget
	StatementResume
	// Operands is Expr

	StatementStash
	StatementRetrieve
	StatementIgnore
	StatementRemember
	// Operands is []interface{}, containing Var16, Var32, Array16, Array32

	StatementAbstainLabel
	// Operands is initially uint16, the label
	// Resolve() changes it to int, the statement index

	StatementAbstainGerundList
	// Operands is initially []TokenType, a list of gerund tokens
	// Resolve() changes it to []int, a list of statement indexes

	StatementReinstateLabel
	// Operands is initially uint16, the label
	// Resolve() changes it to int, the statement index

	StatementReinstateGerundList
	// Operands is initially []TokenType, a list of gerund tokens
	// Resolve() changes it to []int, a list of statement indexes

	StatementGiveUp

	StatementWriteIn
	// Operands is []interface{}, containing Var16, Var32,
	// Array16, Array32, ArrayElement

	StatementReadOut
	// Operands is []interface{}, containing ExprConst, Var16, Var32,
	// ArrayElement
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
				labelTable := make(map[uint16]int)
				gerundTable := make(map[TokenType][]int)
				for i, s := range statements {
					s.Parse(i, labelTable, gerundTable)
				}
				for _, s := range statements {
					s.Resolve(statements, labelTable, gerundTable)
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
		case TokenWow:
			token.Type = TokenSpark
			tokens = append(tokens, token, &Token{
				Filename:    token.Filename,
				LineNum:     token.LineNum,
				ColNum:      token.ColNum,
				Type:        TokenSpot,
				NumberValue: 0,
				StringValue: "",
			})
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

func (s *Statement) Parse(statementIndex int, labelTable map[uint16]int, gerundTable map[TokenType][]int) {
	s.Index = statementIndex
	index := 0
	if len(s.Tokens) >= 3 && s.Tokens[0].Type == TokenWax && s.Tokens[2].Type == TokenWane {
		index = 3
		s.Label = s.Tokens[1].NumberValue
		if s.Label == 0 {
			s.Error = Err197.At(s.Index)
			return
		} else if _, ok := labelTable[s.Label]; ok {
			s.Error = Err182.At(s.Index)
			return
		}
	}

	if index >= len(s.Tokens) {
		return
	}

	if s.Tokens[index].Type == TokenDo {
		index++
	} else if s.Tokens[index].Type == TokenPlease {
		index++
		if index >= len(s.Tokens) {
			return
		}
		if s.Tokens[index].Type == TokenDo {
			index++
		}
	} else {
		return
	}

	{
		haveNot := false
		haveChance := false
		s.Chance = 100
		for {
			if index >= len(s.Tokens) {
				return
			}
			if !haveNot && s.Tokens[index].Type == TokenNot {
				haveNot = true
				index++
				s.Not = true
			} else if !haveChance && s.Tokens[index].Type == TokenDoubleOhSeven {
				haveChance = true
				index++
				if index >= len(s.Tokens) && s.Tokens[index].Type != TokenNumber {
					return
				}
				s.Chance = s.Tokens[index].NumberValue
				index++
			} else {
				break
			}
		}
	}

	if index >= len(s.Tokens) {
		return
	}

	switch s.Tokens[index].Type {
	case TokenSpot, TokenTwoSpot, TokenTail, TokenHybrid:
		// Calculate or CalculateArrayDimension
		statementType, operands, err := s.parseCalculate(index)
		if err != nil {
			s.Error = err
			return
		}
		s.Type = statementType
		s.Operands = operands
		gerundTable[TokenCalculating] = append(gerundTable[TokenCalculating], s.Index)
		return
	case TokenWax:
		// Next
		if index+4 != len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+2].Type != TokenWane || s.Tokens[index+3].Type != TokenNext {
			return
		}
		s.Type = StatementNext
		s.Operands = s.Tokens[index+1].NumberValue
		gerundTable[TokenNexting] = append(gerundTable[TokenNexting], s.Index)
		return
	case TokenForget:
		expr, index, err := s.parseExpr(index, false, TokenString)
		if err != nil {
			s.Error = err
			return
		} else if index < len(s.Tokens) {
			s.Error = Err017
			return
		}
		s.Type = StatementForget
		s.Operands = expr
		gerundTable[TokenForgetting] = append(gerundTable[TokenForgetting], s.Index)
		return
	case TokenResume:
		expr, index, err := s.parseExpr(index, false, TokenString)
		if err != nil {
			s.Error = err
			return
		} else if index < len(s.Tokens) {
			s.Error = Err017
			return
		}
		s.Type = StatementResume
		s.Operands = expr
		gerundTable[TokenResuming] = append(gerundTable[TokenResuming], s.Index)
	case TokenStash, TokenRetrieve, TokenIgnore, TokenRemember:
		list := []interface{}{}
		statementType := StatementStash
		gerund := TokenStashing
		switch s.Tokens[index].Type {
		case TokenStash:
			statementType = StatementStash
			gerund = TokenStashing
		case TokenRetrieve:
			statementType = StatementRetrieve
			gerund = TokenRetrieving
		case TokenIgnore:
			statementType = StatementIgnore
			gerund = TokenIgnoring
		case TokenRemember:
			statementType = StatementRemember
			gerund = TokenRemembering
		}
		index++
		for {
			if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber {
				return
			}
			if s.Tokens[index+1].NumberValue == 0 {
				s.Error = Err200
			}
			switch s.Tokens[index].Type {
			case TokenSpot:
				list = append(list, Var16(s.Tokens[index+1].NumberValue))
			case TokenTwoSpot:
				list = append(list, Var32(s.Tokens[index+1].NumberValue))
			case TokenTail:
				list = append(list, Array16(s.Tokens[index+1].NumberValue))
			case TokenHybrid:
				list = append(list, Array32(s.Tokens[index+1].NumberValue))
			default:
				s.Error = Err200
				return
			}
			index += 2
			if index >= len(s.Tokens) {
				break
			}
			if s.Tokens[index].Type != TokenIntersection {
				return
			}
			index++
		}
		s.Type = statementType
		s.Operands = list
		gerundTable[gerund] = append(gerundTable[gerund], s.Index)
		return
	case TokenAbstain:
		index++
		if index >= len(s.Tokens) || s.Tokens[index].Type != TokenFrom {
			return
		}
		fallthrough
	case TokenReinstate:
		initialIndex := index
		index++
		if index+3 == len(s.Tokens) && s.Tokens[index].Type == TokenWax && s.Tokens[index+1].Type == TokenNumber && s.Tokens[index+2].Type == TokenWane {
			if s.Tokens[initialIndex].Type == TokenReinstate {
				s.Type = StatementReinstateLabel
				s.Operands = s.Tokens[index+1].NumberValue
				gerundTable[TokenReinstating] = append(gerundTable[TokenReinstating], s.Index)
			} else {
				s.Type = StatementAbstainLabel
				s.Operands = s.Tokens[index+1].NumberValue
				gerundTable[TokenAbstaining] = append(gerundTable[TokenAbstaining], s.Index)
			}
			return
		}
		gerunds := []TokenType{}
		for {
			if index >= len(s.Tokens) {
				return
			}
			switch s.Tokens[index].Type {
			case TokenCalculating, TokenNexting, TokenForgetting, TokenResuming, TokenStashing, TokenRetrieving, TokenIgnoring, TokenRemembering, TokenAbstaining, TokenReinstating:
				gerunds = append(gerunds, s.Tokens[index].Type)
			case TokenReading:
				gerunds = append(gerunds, s.Tokens[index].Type)
				if index+1 < len(s.Tokens) && s.Tokens[index+1].Type == TokenOut {
					index++
				}
			case TokenWriting:
				gerunds = append(gerunds, s.Tokens[index].Type)
				if index+1 < len(s.Tokens) && s.Tokens[index+1].Type == TokenIn {
					index++
				}
			default:
				return
			}
			index++
			if index >= len(s.Tokens) {
				if s.Tokens[initialIndex].Type == TokenReinstate {
					s.Type = StatementReinstateGerundList
					s.Operands = gerunds
					gerundTable[TokenReinstating] = append(gerundTable[TokenReinstating], s.Index)
				} else {
					s.Type = StatementAbstainGerundList
					s.Operands = gerunds
					gerundTable[TokenAbstaining] = append(gerundTable[TokenAbstaining], s.Index)
				}
				return
			}
			if s.Tokens[index].Type != TokenIntersection {
				return
			}
			index++
		}
	case TokenGive:
		if index+2 != len(s.Tokens) || s.Tokens[index+1].Type != TokenUp {
			return
		}
		s.Type = StatementGiveUp
		// Don't add to gerundTable since DO ABSTAIN FROM GIVING UP
		// and DO REINSTATE GIVING UP are invalid
		return
	case TokenWrite:
		if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenIn {
			return
		}
		index += 2
		operands := []interface{}{}
		for {
			if index >= len(s.Tokens) {
				return
			}
			switch s.Tokens[index].Type {
			case TokenSpot:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					return
				}
				operands = append(operands, Var16(s.Tokens[index+1].NumberValue))
				index += 2
			case TokenTwoSpot:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					return
				}
				operands = append(operands, Var32(s.Tokens[index+1].NumberValue))
				index += 2
			case TokenTail:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					return
				}
				array := Array16(s.Tokens[index+1].NumberValue)
				index += 2
				if index >= len(s.Tokens) || s.Tokens[index].Type != TokenSub {
					operands = append(operands, array)
				} else {
					subscripts, newIndex, err := s.parseSubscripts(index+1, TokenString)
					if err != nil {
						return
					}
					index = newIndex
					operands = append(operands, ArrayElement{Array: array, Index: subscripts})
				}
			case TokenHybrid:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					return
				}
				array := Array32(s.Tokens[index+1].NumberValue)
				index += 2
				if index >= len(s.Tokens) || s.Tokens[index].Type != TokenSub {
					operands = append(operands, array)
				} else {
					subscripts, newIndex, err := s.parseSubscripts(index+1, TokenString)
					if err != nil {
						return
					}
					index = newIndex
					operands = append(operands, ArrayElement{Array: array, Index: subscripts})
				}
			default:
				return
			}
			if index >= len(s.Tokens) {
				s.Type = StatementWriteIn
				s.Operands = operands
				gerundTable[TokenWriting] = append(gerundTable[TokenWriting], s.Index)
				return
			}
			if s.Tokens[index].Type != TokenIntersection {
				return
			}
			index++
		}
	case TokenRead:
		if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenOut {
			return
		}
		index += 2
		operands := []interface{}{}
		for {
			if index >= len(s.Tokens) {
				return
			}
			switch s.Tokens[index].Type {
			case TokenSpot:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					return
				}
				operands = append(operands, Var16(s.Tokens[index+1].NumberValue))
				index += 2
			case TokenTwoSpot:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					return
				}
				operands = append(operands, Var32(s.Tokens[index+1].NumberValue))
				index += 2
			case TokenTail:
				if index+2 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 || s.Tokens[index+2].Type != TokenSub {
					return
				}
				subscripts, newIndex, err := s.parseSubscripts(index+3, TokenString)
				if err != nil {
					return
				}
				operands = append(operands, ArrayElement{Array: Array16(s.Tokens[index+1].NumberValue), Index: subscripts})
				index = newIndex
			case TokenHybrid:
				if index+2 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 || s.Tokens[index+2].Type != TokenSub {
					return
				}
				subscripts, newIndex, err := s.parseSubscripts(index+3, TokenString)
				if err != nil {
					return
				}
				operands = append(operands, ArrayElement{Array: Array32(s.Tokens[index+1].NumberValue), Index: subscripts})
				index = newIndex
			case TokenMesh:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber {
					return
				}
				operands = append(operands, ExprConst(s.Tokens[index+1].NumberValue))
				index += 2
			default:
				return
			}
			if index >= len(s.Tokens) {
				s.Type = StatementWriteIn
				s.Operands = operands
				gerundTable[TokenWriting] = append(gerundTable[TokenWriting], s.Index)
				return
			}
			if s.Tokens[index].Type != TokenIntersection {
				return
			}
			index++
		}
	default:
		return
	}
}

func (s *Statement) parseCalculate(index int) (StatementType, interface{}, *Error) {
	statementType := StatementUnrecognizable
	var lhs interface{}
	var rhs []Expr
	switch s.Tokens[index].Type {
	case TokenSpot:
		if index+3 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 || s.Tokens[index+2].Type != TokenAngle || s.Tokens[index+3].Type != TokenWorm {
			return statementType, nil, nil
		}
		expr, newIndex, err := s.parseExpr(index+4, false, TokenString)
		if newIndex < len(s.Tokens) || err != nil {
			return statementType, nil, err
		}
		statementType = StatementCalculate
		lhs = Var16(s.Tokens[index+1].NumberValue)
		rhs = []Expr{expr}
	case TokenTwoSpot:
		if index+3 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 || s.Tokens[index+2].Type != TokenAngle || s.Tokens[index+3].Type != TokenWorm {
			return statementType, nil, nil
		}
		expr, newIndex, err := s.parseExpr(index+4, false, TokenString)
		if newIndex < len(s.Tokens) || err != nil {
			return statementType, nil, err
		}
		statementType = StatementCalculate
		lhs = Var32(s.Tokens[index+1].NumberValue)
		rhs = []Expr{expr}
	case TokenTail:
		if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
			return statementType, nil, nil
		}
		if index+3 < len(s.Tokens) && s.Tokens[index+2].Type == TokenAngle && s.Tokens[index+3].Type == TokenWorm {
			dimensions, err := s.parseDimensions(index + 4)
			if err != nil {
				return statementType, nil, err
			}
			statementType = StatementCalculateArrayDimension
			lhs = Array16(s.Tokens[index+1].NumberValue)
			rhs = dimensions
		} else {
			subscripts, newIndex, err := s.parseSubscripts(index, TokenString)
			if err != nil || len(subscripts) == 0 {
				return statementType, nil, err
			}
			if newIndex+2 >= len(s.Tokens) || s.Tokens[newIndex].Type != TokenAngle || s.Tokens[newIndex+1].Type != TokenWorm {
				return statementType, nil, nil
			}
			expr, newIndex2, err := s.parseExpr(newIndex+2, false, TokenString)
			if newIndex2 < len(s.Tokens) || err != nil {
				return statementType, nil, err
			}
			statementType = StatementCalculate
			lhs = ArrayElement{Array: Array16(s.Tokens[index+1].NumberValue), Index: subscripts}
			rhs = []Expr{expr}
		}
	case TokenHybrid:
		if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
			return statementType, nil, nil
		}
		if index+3 < len(s.Tokens) && s.Tokens[index+2].Type == TokenAngle && s.Tokens[index+3].Type == TokenWorm {
			dimensions, err := s.parseDimensions(index + 4)
			if err != nil {
				return statementType, nil, err
			}
			statementType = StatementCalculateArrayDimension
			lhs = Array32(s.Tokens[index+1].NumberValue)
			rhs = dimensions
		} else {
			subscripts, newIndex, err := s.parseSubscripts(index, TokenString)
			if err != nil || len(subscripts) == 0 {
				return statementType, nil, err
			}
			if newIndex+2 >= len(s.Tokens) || s.Tokens[newIndex].Type != TokenAngle || s.Tokens[newIndex+1].Type != TokenWorm {
				return statementType, nil, nil
			}
			expr, newIndex2, err := s.parseExpr(newIndex+2, false, TokenString)
			if newIndex2 < len(s.Tokens) || err != nil {
				return statementType, nil, err
			}
			statementType = StatementCalculate
			lhs = ArrayElement{Array: Array32(s.Tokens[index+1].NumberValue), Index: subscripts}
			rhs = []Expr{expr}
		}
	default:
		return statementType, nil, Err774
	}
	return statementType, Calculation{LHS: lhs, RHS: rhs}, nil
}

func (s *Statement) parseExpr(index int, mustBe16 bool, openGrouper TokenType) (Expr, int, *Error) {
	if index >= len(s.Tokens) {
		return nil, index, Err017
	}
	switch s.Tokens[index].Type {
	case TokenSpot:
		//...
		return nil, index, Err774
	case TokenTwoSpot:
		//...
		return nil, index, Err774
	case TokenTail:
		//...
		return nil, index, Err774
	case TokenHybrid:
		//...
		return nil, index, Err774
	case TokenMesh:
		//...
		return nil, index, Err774
	case TokenSpark:
		//...
		return nil, index, Err774
	case TokenRabbitEars:
		//...
		return nil, index, Err774
	default:
	}
	//...
	return nil, index, Err774
}

func (s *Statement) parseBinaryOp(lhs Expr, index int, mustBe16 bool, openGrouper TokenType) (Expr, int, *Error) {
	if index >= len(s.Tokens) {
		return lhs, index, nil
	}
	switch s.Tokens[index].Type {
	case TokenChange, TokenBigMoney:
		if mustBe16 {
			return lhs, index, nil
		}
		rhs, newIndex, err := s.parseExpr(index+1, false, openGrouper)
		if err != nil {
			return nil, index, err
		}
		return ExprMingle([2]Expr{lhs, rhs}), newIndex, nil
	case TokenSqiggle:
		rhs, newIndex, err := s.parseExpr(index+1, false, openGrouper)
		if err != nil {
			return nil, index, err
		}
		return ExprSelect([2]Expr{lhs, rhs}), newIndex, nil
	default:
		return lhs, index, nil
	}
}

func (s *Statement) parseSubscripts(index int, openGrouper TokenType) ([]Expr, int, *Error) {
	subscripts := []Expr{}
	for {
		expr, newIndex, err := s.parseExpr(index, true, openGrouper)
		if err != nil {
			return subscripts, index, nil
		}
		subscripts = append(subscripts, expr)
		index = newIndex
		if (openGrouper == TokenSpark || openGrouper == TokenRabbitEars) && index < len(s.Tokens) && s.Tokens[index].Type == openGrouper {
			return subscripts, index, nil
		}
	}
}

func (s *Statement) parseDimensions(index int) ([]Expr, *Error) {
	dimensions := []Expr{}
	for {
		if index >= len(s.Tokens) {
			return nil, Err000
		}
		expr, newIndex, err := s.parseExpr(index, false, TokenString)
		if err != nil {
			return nil, err
		}
		dimensions = append(dimensions, expr)
		if newIndex >= len(s.Tokens) {
			return dimensions, nil
		}
		if s.Tokens[newIndex].Type != TokenBy {
			return nil, Err017
		}
		index = newIndex + 1
	}
}

func (s *Statement) Resolve(statements []*Statement, labelTable map[uint16]int, gerundTable map[TokenType][]int) {
	switch s.Type {
	case StatementNext, StatementAbstainLabel, StatementReinstateLabel:
		targetLabel := s.Operands.(uint16)
		if targetIndex, ok := labelTable[targetLabel]; ok {
			s.Operands = targetIndex
			if s.Type == StatementReinstateLabel {
				if statements[targetIndex].Type == StatementGiveUp {
					// Attempting to REINSTATE a GIVE UP statement by line label will have no effect.  Since this REINSTATE statement is not ABSTAINED FROM when it is executing, switch its target to itself.
					s.Operands = s.Index
				}
			}
		} else {
			if s.Type == StatementNext {
				s.Error = Err129
			} else {
				s.Error = Err139
			}
		}
	case StatementAbstainGerundList, StatementReinstateGerundList:
		targetIndexes := []int{}
		for _, gerund := range s.Operands.([]TokenType) {
			for _, targetIndex := range gerundTable[gerund] {
				targetIndexes = append(targetIndexes, targetIndex)
			}
		}
		s.Operands = targetIndexes
	default:
	}
}

func ListStatements(statements []*Statement, out io.Writer) error {
	for _, s := range statements {
		c := ' '
		if s.Type == StatementUnrecognizable {
			c = '*'
		}
		_, err := fmt.Fprintf(out, "%c%4d %s\n", c, s.Index+1, s.String())
		if err != nil {
			return err
		}
	}
	return nil
}

type Calculation struct {
	LHS interface{}
	// LHS is Var16, Var32, Array16, Array32, or ArrayElement

	RHS []Expr
}
