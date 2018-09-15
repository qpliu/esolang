package intercal

import (
	"math/rand"
	"time"
)

type State struct {
	Statements     []*Statement
	StatementIndex int
	NEXTingStack   []int
	Random         *rand.Rand

	var16   map[Var16]*Var
	var32   map[Var32]*Var
	array16 map[Array16]*ArrayVar
	array32 map[Array32]*ArrayVar
}

func NewState(statements []*Statement) *State {
	return &State{
		Statements:     statements,
		StatementIndex: 0,
		NEXTingStack:   make([]int, 0, 79),
		Random:         rand.New(rand.NewSource(time.Now().UnixNano())),

		var16:   make(map[Var16]*Var),
		var32:   make(map[Var32]*Var),
		array16: make(map[Array16]*ArrayVar),
		array32: make(map[Array32]*ArrayVar),
	}
}

func (s *State) Var16(v Var16) *Var {
	val, ok := s.var16[v]
	if !ok {
		val = &Var{}
		s.var16[v] = val
	}
	return val
}

func (s *State) Var32(v Var32) *Var {
	val, ok := s.var32[v]
	if !ok {
		val = &Var{}
		s.var32[v] = val
	}
	return val
}

func (s *State) Array16(v Array16) *ArrayVar {
	val, ok := s.array16[v]
	if !ok {
		val = &ArrayVar{}
		s.array16[v] = val
	}
	return val
}

func (s *State) Array32(v Array32) *ArrayVar {
	val, ok := s.array32[v]
	if !ok {
		val = &ArrayVar{}
		s.array32[v] = val
	}
	return val
}

func (s *State) Run(input *IntercalReader, output *IntercalWriter) *Error {
	for {
		if s.StatementIndex >= len(s.Statements) {
			return Err633
		}
		stmt := s.Statements[s.StatementIndex]
		if stmt.Not {
			if err := s.gotoNext(stmt); err != nil {
				return err.At(s, stmt)
			}
			continue
		}
		if stmt.Please {
			thank := false
			for i := stmt.Index; i < stmt.Index+10 && i < len(s.Statements); i++ {
				thank = thank || s.Statements[i].Thank
			}
			if !thank && s.Random.Intn(100) == 0 {
				if stmt.Type != StatementResume {
					return Err774.At(s, stmt)
				} else {
					stmt.Error = Err774
				}
			}
		}
		runStmtCount := int(stmt.Chance) / 100
		if stmt.Chance%100 > 0 && uint16(s.Random.Intn(100)) >= stmt.Chance%100 {
			runStmtCount++
		}
		if runStmtCount > 0 {
			if stmt.Error != nil && stmt.Type != StatementResume {
				return stmt.Error.At(s, stmt)
			}
			if err := s.runStmt(stmt, input, output, runStmtCount); err != nil {
				if err == ErrGiveUp {
					err = nil
				}
				return err
			}
		} else if err := s.gotoNext(stmt); err != nil {
			return err
		}
	}
}

func (s *State) runStmt(stmt *Statement, input *IntercalReader, output *IntercalWriter, runStmtCount int) *Error {
	switch stmt.Type {
	case StatementCalculate:
		calc := stmt.Operands.(Calculation)
		if !calc.LHS.Ignored(s) {
			for i := 0; i < runStmtCount; i++ {
				val, is16, err := calc.RHS.Eval(s)
				if err != nil {
					return err.At(s, stmt)
				}
				if err := calc.LHS.Gets(s, val, is16); err != nil {
					return err.At(s, stmt)
				}
			}
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementCalculateArrayDimension:
		dim := stmt.Operands.(Dimensioning)
		if dim.LHS.Ignored(s) {
			if err := s.gotoNext(stmt); err != nil {
				return err.At(s, stmt)
			}
			return nil
		}
		dims := []int{}
		for _, e := range dim.RHS {
			v, _, err := e.Eval(s)
			if err != nil {
				return err.At(s, stmt)
			}
			dims = append(dims, int(v))
		}
		if err := dim.LHS.Dimension(s, dims); err != nil {
			return err.At(s, stmt)
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementNext:
		nextIndex, pushNextStack, err := s.getNextStatementIndex(stmt)
		if err != nil {
			return err.At(s, stmt)
		}
		if pushNextStack {
			if len(s.NEXTingStack) >= 79 {
				return Err123.At(s, stmt)
			}
			s.NEXTingStack = append(s.NEXTingStack, stmt.Index+1)
		}
		if len(s.NEXTingStack) >= 79 {
			return Err123.At(s, stmt)
		}
		s.NEXTingStack = append(s.NEXTingStack, nextIndex)
		for i := 1; i < runStmtCount; i++ {
			if len(s.NEXTingStack) >= 79 {
				return Err123.At(s, stmt)
			}
			s.NEXTingStack = append(s.NEXTingStack, stmt.Operands.(int))
		}
		s.StatementIndex = stmt.Operands.(int)
		return nil

	case StatementForget:
		val, _, err := stmt.Operands.(Expr).Eval(s)
		if err != nil {
			return err.At(s, stmt)
		}
		newlen := len(s.NEXTingStack) - int(val)*runStmtCount
		if newlen < 0 {
			newlen = 0
		}
		s.NEXTingStack = s.NEXTingStack[:newlen]
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementResume:
		val, _, err := stmt.Operands.(Expr).Eval(s)
		if err != nil {
			return err.At(s, stmt)
		}
		if val == 0 {
			return Err621.At(s, stmt)
		}
		newlen := len(s.NEXTingStack) - int(val)*runStmtCount
		if newlen < 0 {
			return Err632
		}
		if stmt.Error != nil {
			return stmt.Error.OnTheWayTo(s, stmt, s.NEXTingStack[newlen])
		}
		s.StatementIndex = s.NEXTingStack[newlen]
		s.NEXTingStack = s.NEXTingStack[:newlen]
		return nil

	case StatementStash:
		for i := 0; i < runStmtCount; i++ {
			for _, v := range stmt.Operands.([]Stashable) {
				v.Stash(s)
			}
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementRetrieve:
		for i := 0; i < runStmtCount; i++ {
			for _, v := range stmt.Operands.([]Stashable) {
				if err := v.Retrieve(s); err != nil {
					return err.At(s, stmt)
				}
			}
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementIgnore:
		for _, v := range stmt.Operands.([]Stashable) {
			v.Ignore(s)
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementRemember:
		for _, v := range stmt.Operands.([]Stashable) {
			v.Remember(s)
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementAbstainLabel, StatementAbstainGerundList:
		for _, i := range stmt.Operands.([]int) {
			s.Statements[i].Not = true
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementReinstateLabel, StatementReinstateGerundList:
		for _, i := range stmt.Operands.([]int) {
			s.Statements[i].Not = false
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementGiveUp:
		return ErrGiveUp

	case StatementWriteIn:
		for i := 0; i < runStmtCount; i++ {
			for _, v := range stmt.Operands.([]WriteInable) {
				if v.Ignored(s) {
					continue
				}
				if err := v.WriteIn(s, input); err != nil {
					return err.At(s, stmt)
				}
			}
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementReadOut:
		for i := 0; i < runStmtCount; i++ {
			for _, v := range stmt.Operands.([]ReadOutable) {
				if err := v.ReadOut(s, output); err != nil {
					return err.At(s, stmt)
				}
			}
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementReadOutBit:
		for i := 0; i < runStmtCount; i++ {
			output.WriteBit(stmt.Operands.(bool))
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementWriteIntoBit:
		bit := false
		eof := false
		for i := 0; i < runStmtCount; i++ {
			bit, eof = input.InputBit()
			if eof {
				break
			}
		}
		if eof {
			if err := s.gotoNext(stmt); err != nil {
				return err.At(s, stmt)
			}
		} else if bit {
			s.StatementIndex = stmt.Operands.([2]int)[1]
		} else {
			s.StatementIndex = stmt.Operands.([2]int)[0]
		}
		return nil

	case StatementWriteIntoArray:
		v := s.Array16(stmt.Operands.(Array16))
		if len(v.Value.Values) < 2 {
			return Err241.At(s, stmt)
		}
		eof := false
		bit := false
		initialCount := uint32(0)
		for i := range v.Value.Values {
			v.Value.Values[i] = initialCount
			initialCount = 0
			if eof {
				continue
			}
			if i < len(v.Value.Values)-1 {
				for v.Value.Values[i] < 65535 {
					inBit, inEof := input.InputBit()
					if inEof {
						eof = true
						break
					} else if inBit == bit {
						v.Value.Values[i]++
					} else {
						initialCount = 1
						break
					}
				}
				bit = !bit
			} else {
				for {
					peekBit, peekOk := input.PeekBit()
					if !peekOk || peekBit != bit {
						break
					}
					v.Value.Values[i]++
					input.InputBit()
				}
			}
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementLibrary:
		for i := 0; i < runStmtCount; i++ {
			if err := stmt.Operands.(LibraryFunction).Interp(s); err != nil {
				return err.At(s, stmt)
			}
		}
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	case StatementComeFromLabel, StatementComeFromGerundList, StatementNextFromLabel, StatementNextFromGerundList:
		if err := s.gotoNext(stmt); err != nil {
			return err.At(s, stmt)
		}
		return nil

	default:
		return Err000.At(s, stmt)
	}
}

func (s *State) getNextStatementIndex(stmt *Statement) (int, bool, *Error) {
	if len(stmt.Goto) > 0 {
		gotoChance := uint16(0)
		for _, i := range stmt.Goto {
			target := s.Statements[i]
			if !target.Not {
				gotoChance += target.Chance
			}
			if gotoChance > 100 {
				return 0, false, Err555
			}
		}
		random := uint16(s.Random.Intn(100))
		for _, i := range stmt.Goto {
			target := s.Statements[i]
			if !target.Not {
				if random < target.Chance {
					return target.Index, target.Type == StatementNextFromLabel || target.Type == StatementNextFromGerundList, nil
				}
				random -= target.Chance
			}
		}
	}
	return stmt.Index + 1, false, nil
}

func (s *State) gotoNext(stmt *Statement) *Error {
	nextIndex, pushNextStack, err := s.getNextStatementIndex(stmt)
	if err != nil {
		return err
	}
	if pushNextStack {
		if len(s.NEXTingStack) >= 79 {
			return Err123
		}
		s.NEXTingStack = append(s.NEXTingStack, stmt.Index+1)
	}
	s.StatementIndex = nextIndex
	return nil
}
