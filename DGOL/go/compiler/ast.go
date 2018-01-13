package main

type ASTModule struct {
	Filename   string
	Name       string
	Use        map[string]uint
	Subroutine map[string]ASTRoutine
	Program    *ASTRoutine
}

type ASTRoutine struct {
	Name             string
	LineNumber       uint
	EndLineNumber    uint
	Exported         bool
	ParameterCount   uint
	Vars             []ASTVar
	DoEdgesCount     uint
	CallArgsMaxCount uint
	Statements       []ASTStatement
}

type ASTVar struct {
	Name      string
	Index     uint
	IsCallArg bool
}

type ASTStatement struct {
	Type              ASTStatementType
	LineNumber        uint
	EndLineNumber     uint
	Args              []*ASTVar
	CallTargetModule  string
	CallTargetRoutine string
	IfBranches        []ASTIfBranch
	Statements        []ASTStatement
	DoLoopIndex       uint
	DoEdgesIndex      uint
}

type ASTStatementType int

type ASTIfBranch struct {
	Type       ASTIfBranchType
	LineNumber uint
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
