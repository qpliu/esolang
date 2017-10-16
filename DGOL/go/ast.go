package main

type program struct {
	uses        []string
	subroutines []subroutine
	name        string
	statements  []statement
}

type subroutine struct {
	name       string
	parameters []string
	statements []statement
}

type statement struct {
	stmtType   stmtType
	parameters []string
	statements []statement
}

type stmtType int

const (
	stmtLetEq stmtType = iota
	stmtLetMkEdge
	stmtLetRmEdge
	stmtIf
	stmtIfEq
	stmtIfEdge
	stmtElse
	stmtDoLoop
	stmtDoEdges
	stmtCall
	stmtReturn
	stmtExit
)
