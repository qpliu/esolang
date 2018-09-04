package intercal

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
	Please bool
	Thank  bool

	Type     StatementType
	Operands interface{}
	Index    int
	Error    *Error
}

type StatementType int

const (
	StatementUnrecognizable StatementType = iota

	StatementCalculate
	// Operands is Calculation

	StatementCalculateArrayDimension
	// Operands is Dimensioning

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
	// Operands is []Stashable, containing Var16, Var32, Array16, Array32

	StatementAbstainLabel
	// Operands is initially uint16, the label
	// Resolve() changes it to single element []int, the statement index

	StatementAbstainGerundList
	// Operands is initially []TokenType, a list of gerund tokens
	// Resolve() changes it to []int, a list of statement indexes

	StatementReinstateLabel
	// Operands is initially uint16, the label
	// Resolve() changes it to single element []int, the statement index

	StatementReinstateGerundList
	// Operands is initially []TokenType, a list of gerund tokens
	// Resolve() changes it to []int, a list of statement indexes

	StatementGiveUp

	StatementWriteIn
	// Operands is []WriteInable

	StatementReadOut
	// Operands is []ReadOutable

	StatementReadOutBit
	// Operands is bool

	StatementWriteIntoBit
	// Operands is initially [2]uint16, labels
	// Resolve() changes it to [2]int, statement indexes
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
	thank := false
	for {
		token, err := t.Next()
		if err != nil {
			if err != io.EOF {
				return nil, err
			} else {
				if len(tokens) > 0 {
					statements = append(statements, &Statement{Tokens: tokens, Chance: 100, Thank: thank})
					thank = false
				}
				labelTable := make(map[uint16]int)
				gerundTable := make(map[TokenType][]int)
				for i, s := range statements {
					s.Parse(statements, i, labelTable, gerundTable)
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
				statements = append(statements, &Statement{Tokens: tokens[:len(tokens)-3], Chance: 100, Thank: thank})
				tokens = newTokens
				thank = false
			} else if state == mkStmtStateWaxNumberWane && len(tokens) == 3 {
				tokens = append(tokens, token)
			} else {
				if len(tokens) > 0 {
					statements = append(statements, &Statement{Tokens: tokens, Chance: 100, Thank: thank})
					thank = false
				}
				tokens = []*Token{token}
			}
			if token.Type == TokenPlease {
				state = mkStmtStatePlease
			} else {
				state = mkStmtStateOther
			}
		case TokenWax:
			if statementTakesTrailingLabel(tokens) {
				state = mkStmtStateOther
			} else {
				state = mkStmtStateWax
			}
			tokens = append(tokens, token)
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
				Type:        TokenSpot,
				NumberValue: 0,
				StringValue: "",
			})
		case TokenThank:
			tokens = append(tokens, token)
			thank = true
		default:
			tokens = append(tokens, token)
			if state == mkStmtStateWax {
				state = mkStmtStateWaxNumber
			} else {
				state = mkStmtStateOther
			}
		}
	}
}

func statementTakesTrailingLabel(tokens []*Token) bool {
	i := 0
	if i+2 < len(tokens) && tokens[i].Type == TokenWax && tokens[i+2].Type == TokenWane {
		i += 3
	}

	if i+1 < len(tokens) && tokens[i].Type == TokenPlease && tokens[i+1].Type == TokenDo {
		i += 2
	} else if i < len(tokens) && (tokens[i].Type == TokenPlease || tokens[i].Type == TokenDo) {
		i++
	} else {
		return false
	}

	if i+2 < len(tokens) && tokens[i].Type == TokenNot && tokens[i+1].Type == TokenDoubleOhSeven {
		i += 3
	} else if i+2 < len(tokens) && tokens[i].Type == TokenDoubleOhSeven && tokens[i+2].Type == TokenNot {
		i += 3
	} else if i < len(tokens) && tokens[i].Type == TokenNot {
		i++
	} else if i+1 < len(tokens) && tokens[i].Type == TokenDoubleOhSeven {
		i += 2
	}

	if i+2 == len(tokens) && tokens[i].Type == TokenAbstain && tokens[i+1].Type == TokenFrom {
		return true
	}

	if i+1 == len(tokens) && tokens[i].Type == TokenReinstate {
		return true
	}

	return false
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

func (s *Statement) Parse(statements []*Statement, statementIndex int, labelTable map[uint16]int, gerundTable map[TokenType][]int) {
	s.Index = statementIndex
	index := 0
	if len(s.Tokens) >= 3 && s.Tokens[0].Type == TokenWax && s.Tokens[2].Type == TokenWane {
		index = 3
		s.Label = s.Tokens[1].NumberValue
		if s.Label == 0 {
			s.Error = Err197
			return
		} else if dupIndex, ok := labelTable[s.Label]; ok {
			statements[dupIndex].Error = Err182
			s.Error = Err182
			return
		}
		labelTable[s.Label] = s.Index
	}

	if index >= len(s.Tokens) {
		return
	}

	if s.Tokens[index].Type == TokenDo {
		index++
	} else if s.Tokens[index].Type == TokenPlease {
		s.Please = true
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
				if index >= len(s.Tokens) || s.Tokens[index].Type != TokenNumber {
					return
				}
				s.Chance = s.Tokens[index].NumberValue
				index++
			} else {
				break
			}
		}
		// Don't error if Chance > 100,
		// just give it a chance of executing the
		// statement multiple times.  For example
		//     DO %8000 RESUME #1
		// will have the effect of
		//     PLEASE DO GIVE UP
		// and
		//     PLEASE %8000 (8000) NEXT (8000) DOOR
		// will result in
		//     PROGRAM HAS DISAPPEARED INTO THE BLACK LAGOON.
	}

	switch s.Tokens[index].Type {
	case TokenSpot, TokenTwoSpot, TokenTail, TokenHybrid:
		// Calculate or CalculateArrayDimension
		statementType, operands, err := s.parseCalculate(index)
		if err != nil {
			s.Type = statementType
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
		expr, newIndex, err := s.parseExpr(index+1, false, false, false, TokenString)
		if err != nil {
			s.Type = StatementForget
			s.Error = err
			return
		} else if newIndex < len(s.Tokens) || expr == nil {
			s.Type = StatementForget
			s.Error = Err017
			return
		}
		s.Type = StatementForget
		s.Operands = expr
		gerundTable[TokenForgetting] = append(gerundTable[TokenForgetting], s.Index)
		return
	case TokenResume:
		expr, newIndex, err := s.parseExpr(index+1, false, false, false, TokenString)
		if err != nil {
			s.Error = err
			return
		} else if newIndex < len(s.Tokens) || expr == nil {
			s.Error = Err017
			return
		}
		s.Type = StatementResume
		s.Operands = expr
		gerundTable[TokenResuming] = append(gerundTable[TokenResuming], s.Index)
	case TokenStash, TokenRetrieve, TokenIgnore, TokenRemember:
		list := []Stashable{}
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
				s.Type = statementType
				s.Error = Err200
				return
			}
			index += 2
			if index >= len(s.Tokens) {
				break
			}
			if s.Tokens[index].Type != TokenIntersection {
				s.Type = statementType
				s.Error = Err000
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
			s.Type = StatementAbstainLabel
			s.Error = Err000
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
				s.Type = StatementReinstateGerundList
				s.Error = Err000
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
				s.Type = StatementReinstateGerundList
				s.Error = Err000
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
				s.Type = StatementReinstateGerundList
				s.Error = Err000
				return
			}
			index++
		}
	case TokenGive:
		if index+2 != len(s.Tokens) || s.Tokens[index+1].Type != TokenUp {
			s.Type = StatementGiveUp
			s.Error = Err000
			return
		}
		s.Type = StatementGiveUp
		// Don't add to gerundTable since DO ABSTAIN FROM GIVING UP
		// and DO REINSTATE GIVING UP are invalid
		return
	case TokenWrite:
		if index+7 == len(s.Tokens) && s.Tokens[index+1].Type == TokenInto && s.Tokens[index+2].Type == TokenWax && s.Tokens[index+4].Type == TokenIntersection && s.Tokens[index+6].Type == TokenWane {
			s.Type = StatementWriteIntoBit
			s.Operands = [2]uint16{s.Tokens[index+3].NumberValue, s.Tokens[index+5].NumberValue}
			gerundTable[TokenReading] = append(gerundTable[TokenWriting], s.Index)
			return
		}
		if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenIn {
			s.Type = StatementWriteIn
			s.Error = Err000
			return
		}
		index += 2
		operands := []WriteInable{}
		for {
			if index >= len(s.Tokens) {
				s.Type = StatementWriteIn
				s.Error = Err000
				return
			}
			switch s.Tokens[index].Type {
			case TokenSpot:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					s.Type = StatementWriteIn
					s.Error = Err000
					return
				}
				operands = append(operands, Var16(s.Tokens[index+1].NumberValue))
				index += 2
			case TokenTwoSpot:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					s.Type = StatementWriteIn
					s.Error = Err000
					return
				}
				operands = append(operands, Var32(s.Tokens[index+1].NumberValue))
				index += 2
			case TokenTail:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					s.Type = StatementWriteIn
					s.Error = Err000
					return
				}
				array := Array16(s.Tokens[index+1].NumberValue)
				index += 2
				if index >= len(s.Tokens) || s.Tokens[index].Type != TokenSub {
					operands = append(operands, array)
				} else {
					subscripts, newIndex, err := s.parseSubscripts(index+1, TokenString)
					if err != nil {
						s.Type = StatementWriteIn
						s.Error = err
						return
					}
					if len(subscripts) == 0 {
						s.Type = StatementWriteIn
						s.Error = Err017
						return
					}
					index = newIndex
					operands = append(operands, ArrayElement{Array: array, Index: subscripts})
				}
			case TokenHybrid:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					s.Type = StatementWriteIn
					s.Error = Err000
					return
				}
				array := Array32(s.Tokens[index+1].NumberValue)
				index += 2
				if index >= len(s.Tokens) || s.Tokens[index].Type != TokenSub {
					operands = append(operands, array)
				} else {
					subscripts, newIndex, err := s.parseSubscripts(index+1, TokenString)
					if err != nil {
						s.Type = StatementWriteIn
						s.Error = err
						return
					}
					if len(subscripts) == 0 {
						s.Type = StatementWriteIn
						s.Error = Err017
						return
					}
					index = newIndex
					operands = append(operands, ArrayElement{Array: array, Index: subscripts})
				}
			default:
				s.Type = StatementWriteIn
				s.Error = Err000
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
		if index+2 == len(s.Tokens) && s.Tokens[index+1].Type == TokenOut {
			s.Type = StatementReadOutBit
			s.Operands = true
			gerundTable[TokenReading] = append(gerundTable[TokenReading], s.Index)
			return
		}
		if index+2 == len(s.Tokens) && (s.Tokens[index+1].Type == TokenNaught || s.Tokens[index+1].Type == TokenNot) {
			s.Type = StatementReadOutBit
			s.Operands = false
			gerundTable[TokenReading] = append(gerundTable[TokenReading], s.Index)
			return
		}

		if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenOut {
			s.Type = StatementReadOut
			s.Error = Err000
			return
		}
		index += 2
		operands := []ReadOutable{}
		for {
			if index >= len(s.Tokens) {
				s.Type = StatementReadOut
				s.Error = Err000
				return
			}
			switch s.Tokens[index].Type {
			case TokenSpot:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					s.Type = StatementReadOut
					s.Error = Err000
					return
				}
				operands = append(operands, Var16(s.Tokens[index+1].NumberValue))
				index += 2
			case TokenTwoSpot:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
					s.Type = StatementReadOut
					s.Error = Err000
					return
				}
				operands = append(operands, Var32(s.Tokens[index+1].NumberValue))
				index += 2
			case TokenTail:
				if index+2 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 || s.Tokens[index+2].Type != TokenSub {
					s.Type = StatementReadOut
					s.Error = Err000
					return
				}
				subscripts, newIndex, err := s.parseSubscripts(index+3, TokenString)
				if err != nil {
					s.Type = StatementReadOut
					s.Error = err
					return
				}
				if len(subscripts) == 0 {
					s.Type = StatementReadOut
					s.Error = Err017
					return
				}
				operands = append(operands, ArrayElement{Array: Array16(s.Tokens[index+1].NumberValue), Index: subscripts})
				index = newIndex
			case TokenHybrid:
				if index+2 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 || s.Tokens[index+2].Type != TokenSub {
					s.Type = StatementReadOut
					s.Error = Err000
					return
				}
				subscripts, newIndex, err := s.parseSubscripts(index+3, TokenString)
				if err != nil {
					s.Type = StatementReadOut
					s.Error = err
					return
				}
				if len(subscripts) == 0 {
					s.Type = StatementReadOut
					s.Error = Err017
					return
				}
				operands = append(operands, ArrayElement{Array: Array32(s.Tokens[index+1].NumberValue), Index: subscripts})
				index = newIndex
			case TokenMesh:
				if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber {
					s.Type = StatementReadOut
					s.Error = Err000
					return
				}
				operands = append(operands, ExprConst(s.Tokens[index+1].NumberValue))
				index += 2
			default:
				s.Type = StatementReadOut
				s.Error = Err000
				return
			}
			if index >= len(s.Tokens) {
				s.Type = StatementReadOut
				s.Operands = operands
				gerundTable[TokenReading] = append(gerundTable[TokenReading], s.Index)
				return
			}
			if s.Tokens[index].Type != TokenIntersection {
				s.Type = StatementReadOut
				s.Error = Err000
				return
			}
			index++
		}
	default:
		return
	}
}

func (s *Statement) parseCalculate(index int) (StatementType, interface{}, *Error) {
	switch s.Tokens[index].Type {
	case TokenSpot:
		if index+3 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 || s.Tokens[index+2].Type != TokenAngle || s.Tokens[index+3].Type != TokenWorm {
			return StatementUnrecognizable, nil, nil
		}
		expr, newIndex, err := s.parseExpr(index+4, false, false, false, TokenString)
		if err != nil {
			return StatementCalculate, nil, err
		} else if newIndex < len(s.Tokens) || expr == nil {
			return StatementCalculate, nil, Err017
		}
		return StatementCalculate, Calculation{LHS: Var16(s.Tokens[index+1].NumberValue), RHS: expr}, nil
	case TokenTwoSpot:
		if index+3 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 || s.Tokens[index+2].Type != TokenAngle || s.Tokens[index+3].Type != TokenWorm {
			return StatementUnrecognizable, nil, nil
		}
		expr, newIndex, err := s.parseExpr(index+4, false, false, false, TokenString)
		if err != nil {
			return StatementCalculate, nil, err
		} else if newIndex < len(s.Tokens) || expr == nil {
			return StatementCalculate, nil, Err017
		}
		return StatementCalculate, Calculation{LHS: Var32(s.Tokens[index+1].NumberValue), RHS: expr}, nil
	case TokenTail:
		if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
			return StatementUnrecognizable, nil, nil
		}
		if index+3 < len(s.Tokens) && s.Tokens[index+2].Type == TokenAngle && s.Tokens[index+3].Type == TokenWorm {
			dimensions, err := s.parseDimensions(index + 4)
			if err != nil {
				return StatementCalculate, nil, err
			}
			return StatementCalculateArrayDimension, Dimensioning{LHS: Array16(s.Tokens[index+1].NumberValue), RHS: dimensions}, nil
		} else if index+2 < len(s.Tokens) && s.Tokens[index+2].Type == TokenSub {
			subscripts, newIndex, err := s.parseSubscripts(index+3, TokenString)
			if err != nil {
				return StatementCalculate, nil, err
			}
			if len(subscripts) == 0 {
				return StatementCalculate, nil, Err017
			}
			if newIndex+2 >= len(s.Tokens) || s.Tokens[newIndex].Type != TokenAngle || s.Tokens[newIndex+1].Type != TokenWorm {
				return StatementCalculate, nil, Err000
			}
			expr, newIndex2, err := s.parseExpr(newIndex+2, false, false, false, TokenString)
			if err != nil {
				return StatementCalculate, nil, err
			} else if newIndex2 < len(s.Tokens) {
				return StatementCalculate, nil, Err000
			} else if expr == nil {
				return StatementCalculate, nil, Err017
			}
			return StatementCalculate, Calculation{LHS: ArrayElement{Array: Array16(s.Tokens[index+1].NumberValue), Index: subscripts}, RHS: expr}, nil
		} else {
			return StatementUnrecognizable, nil, nil
		}
	case TokenHybrid:
		if index+1 >= len(s.Tokens) || s.Tokens[index+1].Type != TokenNumber || s.Tokens[index+1].NumberValue == 0 {
			return StatementUnrecognizable, nil, nil
		}
		if index+3 < len(s.Tokens) && s.Tokens[index+2].Type == TokenAngle && s.Tokens[index+3].Type == TokenWorm {
			dimensions, err := s.parseDimensions(index + 4)
			if err != nil {
				return StatementCalculate, nil, err
			}
			return StatementCalculateArrayDimension, Dimensioning{LHS: Array32(s.Tokens[index+1].NumberValue), RHS: dimensions}, nil
		} else if index+2 < len(s.Tokens) && s.Tokens[index+2].Type == TokenSub {
			subscripts, newIndex, err := s.parseSubscripts(index+3, TokenString)
			if err != nil {
				return StatementCalculate, nil, err
			}
			if len(subscripts) == 0 {
				return StatementCalculate, nil, Err017
			}
			if newIndex+2 >= len(s.Tokens) || s.Tokens[newIndex].Type != TokenAngle || s.Tokens[newIndex+1].Type != TokenWorm {
				return StatementCalculate, nil, Err000
			}
			expr, newIndex2, err := s.parseExpr(newIndex+2, false, false, false, TokenString)
			if err != nil {
				return StatementCalculate, nil, err
			} else if newIndex2 < len(s.Tokens) {
				return StatementCalculate, nil, Err000
			} else if expr == nil {
				return StatementCalculate, nil, Err017
			}
			return StatementCalculate, Calculation{LHS: ArrayElement{Array: Array32(s.Tokens[index+1].NumberValue), Index: subscripts}, RHS: expr}, nil
		} else {
			return StatementUnrecognizable, nil, nil
		}
	default:
		return StatementUnrecognizable, nil, Err774
	}
}

func (s *Statement) parseExpr(index int, mustBe16, binaryOpRhs, firstSubscript bool, openGrouper TokenType) (Expr, int, *Error) {
	if index >= len(s.Tokens) {
		return nil, index, nil
	}
	var result Expr
	var unaryOp *Token
	switch s.Tokens[index].Type {
	case TokenSpot:
		index++
		if index < len(s.Tokens) && s.Tokens[index].IsUnaryOp() {
			unaryOp = s.Tokens[index]
			index++
		}
		if index >= len(s.Tokens) || s.Tokens[index].Type != TokenNumber {
			return nil, index, Err017
		}
		result = Var16(s.Tokens[index].NumberValue)
		index++
	case TokenTwoSpot:
		index++
		if index < len(s.Tokens) && s.Tokens[index].IsUnaryOp() {
			unaryOp = s.Tokens[index]
			index++
		}
		if index >= len(s.Tokens) || s.Tokens[index].Type != TokenNumber {
			return nil, index, Err017
		}
		result = Var32(s.Tokens[index].NumberValue)
		index++
	case TokenTail:
		index++
		if index < len(s.Tokens) && s.Tokens[index].IsUnaryOp() {
			unaryOp = s.Tokens[index]
			index++
		}
		if index+1 >= len(s.Tokens) || s.Tokens[index].Type != TokenNumber || s.Tokens[index].NumberValue == 0 || s.Tokens[index+1].Type != TokenSub {
			return nil, index, Err017
		}
		subscripts, newIndex, err := s.parseSubscripts(index+2, openGrouper)
		if err != nil {
			return nil, index, err
		} else if len(subscripts) == 0 {
			return nil, index, Err017
		}
		result = ArrayElement{Array: Array16(s.Tokens[index].NumberValue), Index: subscripts}
		index = newIndex
	case TokenHybrid:
		index++
		if index < len(s.Tokens) && s.Tokens[index].IsUnaryOp() {
			unaryOp = s.Tokens[index]
			index++
		}
		if index+1 >= len(s.Tokens) || s.Tokens[index].Type != TokenNumber || s.Tokens[index].NumberValue == 0 || s.Tokens[index+1].Type != TokenSub {
			return nil, index, Err017
		}
		subscripts, newIndex, err := s.parseSubscripts(index+2, openGrouper)
		if err != nil {
			return nil, index, err
		} else if len(subscripts) == 0 {
			return nil, index, Err017
		}
		result = ArrayElement{Array: Array32(s.Tokens[index].NumberValue), Index: subscripts}
		index = newIndex
	case TokenMesh:
		index++
		if index < len(s.Tokens) && s.Tokens[index].IsUnaryOp() {
			unaryOp = s.Tokens[index]
			index++
		}
		if index >= len(s.Tokens) || s.Tokens[index].Type != TokenNumber {
			return nil, index, Err017
		}
		result = ExprConst(s.Tokens[index].NumberValue)
		index++
	case TokenSpark:
		if openGrouper == TokenSpark && (index+1 >= len(s.Tokens) || !s.Tokens[index+1].IsUnaryOp()) && !binaryOpRhs && !firstSubscript {
			return nil, index, nil
		}
		index++
		if s.Tokens[index].IsUnaryOp() {
			unaryOp = s.Tokens[index]
			index++
		}
		expr, newIndex, err := s.parseExpr(index, false, false, false, TokenSpark)
		if err != nil {
			return nil, index, err
		} else if expr == nil || newIndex >= len(s.Tokens) || s.Tokens[newIndex].Type != TokenSpark {
			return nil, index, Err017
		}
		index = newIndex + 1
		result = expr
	case TokenRabbitEars:
		if openGrouper == TokenRabbitEars && (index+1 >= len(s.Tokens) || !s.Tokens[index+1].IsUnaryOp()) && !binaryOpRhs && !firstSubscript {
			return nil, index, nil
		}
		index++
		if s.Tokens[index].IsUnaryOp() {
			unaryOp = s.Tokens[index]
			index++
		}
		expr, newIndex, err := s.parseExpr(index, false, false, false, TokenRabbitEars)
		if err != nil {
			return nil, index, err
		} else if expr == nil || newIndex >= len(s.Tokens) || s.Tokens[newIndex].Type != TokenRabbitEars {
			return nil, index, Err017
		}
		index = newIndex + 1
		result = expr
	default:
		return nil, index, Err017
	}

	if unaryOp != nil {
		switch unaryOp.Type {
		case TokenAmpersand:
			result = ExprAnd([1]Expr{result})
		case TokenBook:
			result = ExprOr([1]Expr{result})
		case TokenBookworm, TokenWhat:
			result = ExprXor([1]Expr{result})
		}
	}

	if !binaryOpRhs {
		return s.parseBinaryOp(result, index, mustBe16, openGrouper)
	} else {
		return result, index, nil
	}
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
		rhs, newIndex, err := s.parseExpr(index+1, false, true, false, openGrouper)
		if err != nil {
			return nil, index, err
		} else if rhs == nil {
			return nil, index, Err017
		}
		return ExprMingle([2]Expr{lhs, rhs}), newIndex, nil
	case TokenSqiggle:
		rhs, newIndex, err := s.parseExpr(index+1, false, true, false, openGrouper)
		if err != nil {
			return nil, index, err
		} else if rhs == nil {
			return nil, index, Err017
		}
		return ExprSelect([2]Expr{lhs, rhs}), newIndex, nil
	default:
		return lhs, index, nil
	}
}

func (s *Statement) parseSubscripts(index int, openGrouper TokenType) ([]Expr, int, *Error) {
	subscripts := []Expr{}
	for {
		expr, newIndex, err := s.parseExpr(index, true, false, len(subscripts) == 0, openGrouper)
		if err != nil || expr == nil {
			return subscripts, index, nil
		}
		subscripts = append(subscripts, expr)
		index = newIndex
		if (openGrouper == TokenSpark || openGrouper == TokenRabbitEars) && index < len(s.Tokens) && s.Tokens[index].Type == openGrouper {
			if index+1 < len(s.Tokens) && !s.Tokens[index+1].IsUnaryOp() {
				return subscripts, index, nil
			}
		}
	}
}

func (s *Statement) parseDimensions(index int) ([]Expr, *Error) {
	dimensions := []Expr{}
	for {
		if index >= len(s.Tokens) {
			return nil, Err017
		}
		expr, newIndex, err := s.parseExpr(index, false, false, false, TokenString)
		if err != nil {
			return nil, err
		} else if expr == nil {
			return nil, Err017
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
	if s.Error != nil {
		return
	}
	switch s.Type {
	case StatementNext, StatementAbstainLabel, StatementReinstateLabel:
		targetLabel := s.Operands.(uint16)
		if targetIndex, ok := labelTable[targetLabel]; ok {
			if s.Type == StatementNext {
				s.Operands = targetIndex
			} else {
				if s.Type == StatementReinstateLabel && statements[targetIndex].Type == StatementGiveUp {
					// Attempting to REINSTATE a GIVE UP statement by line label will have no effect.
					s.Operands = []int{}
				} else {
					s.Operands = []int{targetIndex}
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
	case StatementWriteIntoBit:
		labels := s.Operands.([2]uint16)
		if labels[0] == 0 || labels[1] == 0 {
			s.Error = Err197
			break
		}
		index0, ok := labelTable[labels[0]]
		if !ok {
			s.Error = Err139
			break
		}
		index1, ok := labelTable[labels[1]]
		if !ok {
			s.Error = Err139
			break
		}
		s.Operands = [2]int{index0, index1}
	default:
	}
}

func ListStatements(statements []*Statement, out io.Writer) error {
	for _, s := range statements {
		c := ' '
		if s.Type == StatementUnrecognizable {
			c = '*'
		}
		_, err := fmt.Fprintf(out, "%c%04d %s\n", c, s.Index+1, s.String())
		if err != nil {
			return err
		}
	}
	return nil
}

func Strict(statements []*Statement) {
	for _, stmt := range statements {
		if stmt.Chance == 0 || stmt.Chance >= 100 || stmt.Type == StatementReadOutBit || stmt.Type == StatementWriteIntoBit {
			stmt.Error = Err000
		}
	}
}

type Calculation struct {
	LHS LValue
	RHS Expr
}

type Dimensioning struct {
	LHS Dimensionable
	RHS []Expr
}
