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
			s.StatementIndex = stmt.Index + 1
			continue
		}
		if stmt.Please {
			thank := false
			for i := stmt.Index + 1; i < stmt.Index+10 && i < len(s.Statements); i++ {
				thank = thank || s.Statements[i].Thank
			}
			if !thank && s.Random.Intn(100) == 0 {
				return Err774.At(s, stmt)
			}
		}
		chance := stmt.Chance
		for {
			if chance == 0 || (chance < 100 && uint16(s.Random.Intn(100)) >= chance) {
				if chance == stmt.Chance {
					s.StatementIndex = stmt.Index + 1
				}
				break
			}
			if stmt.Error != nil {
				return stmt.Error.At(s, stmt)
			}
			if err := s.runStmt(stmt, input, output); err != nil {
				if err == ErrGiveUp {
					err = nil
				}
				return err
			}
			if chance <= 100 {
				break
			}
			chance -= 100
		}
	}
}

func (s *State) runStmt(stmt *Statement, input *IntercalReader, output *IntercalWriter) *Error {
	switch stmt.Type {
	case StatementCalculate:
		calc := stmt.Operands.(Calculation)
		if calc.LHS.Ignored(s) {
			s.StatementIndex = stmt.Index + 1
			return nil
		}
		val, is16, err := calc.RHS.Eval(s)
		if err != nil {
			return err.At(s, stmt)
		}
		if err := calc.LHS.Gets(s, val, is16); err != nil {
			return err.At(s, stmt)
		}
		s.StatementIndex = stmt.Index + 1
		return nil

	case StatementCalculateArrayDimension:
		dim := stmt.Operands.(Dimensioning)
		if dim.LHS.Ignored(s) {
			s.StatementIndex = stmt.Index + 1
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
		s.StatementIndex = stmt.Index + 1
		return nil

	case StatementNext:
		if len(s.NEXTingStack) >= 79 {
			return Err123.At(s, stmt)
		}
		s.NEXTingStack = append(s.NEXTingStack, stmt.Index+1)
		s.StatementIndex = stmt.Operands.(int)
		return nil

	case StatementForget:
		val, _, err := stmt.Operands.(Expr).Eval(s)
		if err != nil {
			return err.At(s, stmt)
		}
		newlen := len(s.NEXTingStack) - int(val)
		if newlen < 0 {
			newlen = 0
		}
		s.NEXTingStack = s.NEXTingStack[:newlen]
		s.StatementIndex = stmt.Index + 1
		return nil

	case StatementResume:
		val, _, err := stmt.Operands.(Expr).Eval(s)
		if err != nil {
			return err.At(s, stmt)
		}
		if val == 0 {
			return Err621.At(s, stmt)
		}
		newlen := len(s.NEXTingStack) - int(val)
		if newlen < 0 {
			return Err632
		}
		s.StatementIndex = s.NEXTingStack[newlen]
		s.NEXTingStack = s.NEXTingStack[:newlen]
		return nil

	case StatementStash:
		for _, v := range stmt.Operands.([]Stashable) {
			v.Stash(s)
		}
		s.StatementIndex = stmt.Index + 1
		return nil

	case StatementRetrieve:
		for _, v := range stmt.Operands.([]Stashable) {
			if err := v.Retrieve(s); err != nil {
				return err.At(s, stmt)
			}
		}
		s.StatementIndex = stmt.Index + 1
		return nil

	case StatementIgnore:
		for _, v := range stmt.Operands.([]Stashable) {
			v.Ignore(s)
		}
		s.StatementIndex = stmt.Index + 1
		return nil

	case StatementRemember:
		for _, v := range stmt.Operands.([]Stashable) {
			v.Remember(s)
		}
		s.StatementIndex = stmt.Index + 1
		return nil

	case StatementAbstainLabel, StatementAbstainGerundList:
		for _, i := range stmt.Operands.([]int) {
			s.Statements[i].Not = true
		}
		s.StatementIndex = stmt.Index + 1
		return nil

	case StatementReinstateLabel, StatementReinstateGerundList:
		for _, i := range stmt.Operands.([]int) {
			s.Statements[i].Not = false
		}
		s.StatementIndex = stmt.Index + 1
		return nil

	case StatementGiveUp:
		return ErrGiveUp

	case StatementWriteIn:
		for _, v := range stmt.Operands.([]WriteInable) {
			if v.Ignored(s) {
				continue
			}
			if err := v.WriteIn(s, input); err != nil {
				return err.At(s, stmt)
			}
		}
		s.StatementIndex = stmt.Index + 1
		return nil

	case StatementReadOut:
		for _, v := range stmt.Operands.([]ReadOutable) {
			if err := v.ReadOut(s, output); err != nil {
				return err.At(s, stmt)
			}
		}
		s.StatementIndex = stmt.Index + 1
		return nil

	case StatementReadOutBit:
		output.WriteBit(stmt.Operands.(bool))
		s.StatementIndex = stmt.Index + 1
		return nil

	default:
		return Err000.At(s, stmt)
	}
}
