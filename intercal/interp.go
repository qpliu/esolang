package main

type State struct {
	Statements     []*Statement
	StatementIndex int
	NEXTingStack   []int

	Var16   map[Var16]*Var
	Var32   map[Var32]*Var
	Array16 map[Array16]*ArrayVar
	Array32 map[Array32]*ArrayVar
}
