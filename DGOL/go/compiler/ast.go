package main

type ASTModule struct {
	Filename   string
	Name       string
	Use        map[string]int
	Subroutine map[string]ASTRoutine
	Program    *ASTRoutine
}

type ASTRoutine struct {
	Name             string
	LineNumber       int
	EndLineNumber    int
	Exported         bool
	ParameterCount   int
	Vars             []ASTVar
	DoEdgesCount     int
	CallArgsMaxCount int
	Statements       []ASTStatement
}

type ASTVar struct {
	Name      string
	Index     int
	IsCallArg bool
}

type ASTStatement struct {
	Type              ASTStatementType
	LineNumber        int
	EndLineNumber     int
	Args              []*ASTVar
	CallTargetModule  string
	CallTargetRoutine string
	IfBranches        []ASTIfBranch
	Statements        []ASTStatement
	DoLoopIndex       int
	DoEdgesIndex      int
}

type ASTStatementType int

type ASTIfBranch struct {
	Type       ASTIfBranchType
	LineNumber int
	Args       []ASTVar
	Statements []ASTStatement
}

type ASTIfBranchType int

const (
	StmtLetEq ASTStatementType = iota
	StmtLetAddEdge
	StmtLetRemoveEdge
	StmtIf
	StmtCall
	StmtReturn
	StmtDoLoop
	StmtDoEdges
	StmtExit

	IfBranchEq ASTIfBranchType = iota
	IfBranchEdge
	IfBranchElse
)
