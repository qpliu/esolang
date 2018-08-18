package main

type State struct {
	Statements     []*Statement
	StatementIndex int
	NEXTingStack   []int

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

		var16:   make(map[Var16]*Var),
		var32:   make(map[Var32]*Var),
		array16: make(map[Array16]*ArrayVar),
		array32: make(map[Array32]*ArrayVar),
	}
}

func (s *State) Var16(v Var16) *Var {
	val, ok := s.var16[v]
	if !ok {
		val := &Var{}
		s.var16[v] = val
	}
	return val
}

func (s *State) Var32(v Var32) *Var {
	val, ok := s.var32[v]
	if !ok {
		val := &Var{}
		s.var32[v] = val
	}
	return val
}

func (s *State) Array16(v Array16) *ArrayVar {
	val, ok := s.array16[v]
	if !ok {
		val := &ArrayVar{}
		s.array16[v] = val
	}
	return val
}

func (s *State) Array32(v Array32) *ArrayVar {
	val, ok := s.array32[v]
	if !ok {
		val := &ArrayVar{}
		s.array32[v] = val
	}
	return val
}
