package main

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func testParse(t *testing.T, code string) *ASTModule {
	module, err := Parse("<test>", bytes.NewBufferString(code))
	assert.NoError(t, err)
	return module
}

func testParseError(t *testing.T, code string, errorMessage string) {
	_, err := Parse("<test>", bytes.NewBufferString(code))
	assert.Error(t, err)
	assert.Equal(t, errorMessage, err.Error())
}

func TestParse(t *testing.T) {
	module := testParse(t, `PROGRAM TEST
END TEST`)
	assert.Equal(t, "TEST", module.Name)
	assert.Len(t, module.Use, 0)
	assert.Len(t, module.Subroutine, 0)
	assert.NotNil(t, module.Program)
	assert.Equal(t, "TEST", module.Program.Name)
	assert.Equal(t, 1, module.Program.LineNumber)
	assert.Equal(t, 2, module.Program.EndLineNumber)
	assert.False(t, module.Program.Exported)
	assert.Equal(t, 0, module.Program.ParameterCount)
	assert.Len(t, module.Program.Vars, 0)
	assert.Equal(t, 0, module.Program.DoEdgesCount)
	assert.Equal(t, 0, module.Program.CallArgsMaxCount)
	assert.Len(t, module.Program.Statements, 0)
}

func TestParseComments(t *testing.T) {
	module := testParse(t, `PROGRAM TEST * LINE END COMMENT
* FULL LINE COMMENT
END TEST
* TRAILING COMMENT`)
	assert.Equal(t, "TEST", module.Name)
	assert.Len(t, module.Use, 0)
	assert.Len(t, module.Subroutine, 0)
	assert.NotNil(t, module.Program)
	assert.Equal(t, "TEST", module.Program.Name)
	assert.Equal(t, 1, module.Program.LineNumber)
	assert.Equal(t, 3, module.Program.EndLineNumber)
	assert.False(t, module.Program.Exported)
	assert.Equal(t, 0, module.Program.ParameterCount)
	assert.Len(t, module.Program.Vars, 0)
	assert.Equal(t, 0, module.Program.DoEdgesCount)
	assert.Equal(t, 0, module.Program.CallArgsMaxCount)
	assert.Len(t, module.Program.Statements, 0)
}

func TestParseUse(t *testing.T) {
	module := testParse(t, `USE T
PROGRAM TEST
END TEST`)
	assert.Equal(t, "TEST", module.Name)
	assert.Len(t, module.Use, 1)
	assert.Equal(t, 1, module.Use["T"])
	assert.Len(t, module.Subroutine, 0)
	assert.NotNil(t, module.Program)
	assert.Equal(t, "TEST", module.Program.Name)
	assert.Equal(t, 2, module.Program.LineNumber)
	assert.Equal(t, 3, module.Program.EndLineNumber)
	assert.False(t, module.Program.Exported)
	assert.Equal(t, 0, module.Program.ParameterCount)
	assert.Len(t, module.Program.Vars, 0)
	assert.Equal(t, 0, module.Program.DoEdgesCount)
	assert.Equal(t, 0, module.Program.CallArgsMaxCount)
	assert.Len(t, module.Program.Statements, 0)
}

func TestParseLibrary(t *testing.T) {
	module := testParse(t, `SUBROUTINE A()
END A
SUBROUTINE B()
END B
LIBRARY TEST
  SUBROUTINE A
END TEST`)
	assert.Equal(t, "TEST", module.Name)
	assert.Len(t, module.Use, 0)
	assert.Len(t, module.Subroutine, 2)
	assert.Nil(t, module.Program)
	a, ok := module.Subroutine["A"]
	assert.True(t, ok)
	b, ok := module.Subroutine["B"]
	assert.True(t, ok)
	_, ok = module.Subroutine["C"]
	assert.False(t, ok)

	assert.Equal(t, "A", a.Name)
	assert.Equal(t, 1, a.LineNumber)
	assert.Equal(t, 2, a.EndLineNumber)
	assert.True(t, a.Exported)
	assert.Equal(t, 0, a.ParameterCount)
	assert.Len(t, a.Vars, 0)
	assert.Equal(t, 0, a.DoEdgesCount)
	assert.Equal(t, 0, a.CallArgsMaxCount)
	assert.Len(t, a.Statements, 0)

	assert.Equal(t, "B", b.Name)
	assert.Equal(t, 3, b.LineNumber)
	assert.Equal(t, 4, b.EndLineNumber)
	assert.False(t, b.Exported)
	assert.Equal(t, 0, b.ParameterCount)
	assert.Len(t, b.Vars, 0)
	assert.Equal(t, 0, b.DoEdgesCount)
	assert.Equal(t, 0, b.CallArgsMaxCount)
	assert.Len(t, b.Statements, 0)
}

func TestParseParameter(t *testing.T) {
	module := testParse(t, `SUBROUTINE A(X)
LET X > Y
END A
PROGRAM T
END T`)
	assert.Equal(t, "T", module.Name)
	assert.Len(t, module.Use, 0)
	assert.Len(t, module.Subroutine, 1)
	assert.NotNil(t, module.Program)
	assert.Equal(t, "T", module.Program.Name)
	assert.Equal(t, 4, module.Program.LineNumber)
	assert.Equal(t, 5, module.Program.EndLineNumber)
	assert.False(t, module.Program.Exported)
	assert.Equal(t, 0, module.Program.ParameterCount)
	assert.Len(t, module.Program.Vars, 0)
	assert.Equal(t, 0, module.Program.DoEdgesCount)
	assert.Equal(t, 0, module.Program.CallArgsMaxCount)
	assert.Len(t, module.Program.Statements, 0)

	a, ok := module.Subroutine["A"]
	assert.True(t, ok)
	assert.Equal(t, "A", a.Name)
	assert.Equal(t, 1, a.LineNumber)
	assert.Equal(t, 3, a.EndLineNumber)
	assert.False(t, a.Exported)
	assert.Equal(t, 1, a.ParameterCount)
	assert.Len(t, a.Vars, 2)
	assert.Equal(t, 0, a.DoEdgesCount)
	assert.Equal(t, 0, a.CallArgsMaxCount)
	assert.Len(t, a.Statements, 1)

	assert.Equal(t, "X", a.Vars[0].Name)
	assert.Equal(t, 0, a.Vars[0].Index)
	assert.True(t, a.Vars[0].IsCallArg)

	assert.Equal(t, "Y", a.Vars[1].Name)
	assert.Equal(t, 1, a.Vars[1].Index)
	assert.False(t, a.Vars[1].IsCallArg)
}

func TestParseLet(t *testing.T) {
	module := testParse(t, `PROGRAM T
LET A = A
LET A = 0
LET A > A
LET A > 0
LET A < A
END T`)
	assert.Equal(t, "T", module.Name)
	assert.Len(t, module.Use, 0)
	assert.Len(t, module.Subroutine, 0)
	assert.NotNil(t, module.Program)
	assert.Equal(t, "T", module.Program.Name)
	assert.Equal(t, 1, module.Program.LineNumber)
	assert.Equal(t, 7, module.Program.EndLineNumber)
	assert.False(t, module.Program.Exported)
	assert.Equal(t, 0, module.Program.ParameterCount)
	assert.Len(t, module.Program.Vars, 1)
	assert.Equal(t, 0, module.Program.DoEdgesCount)
	assert.Equal(t, 0, module.Program.CallArgsMaxCount)
	assert.Len(t, module.Program.Statements, 5)

	assert.Equal(t, StmtLetEq, module.Program.Statements[0].Type)
	assert.Equal(t, 2, module.Program.Statements[0].LineNumber)
	assert.Len(t, module.Program.Statements[0].Args, 2)
	assert.NotNil(t, module.Program.Statements[0].Args[0])
	assert.Equal(t, "A", module.Program.Statements[0].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[0].Args[0].Index)
	assert.False(t, module.Program.Statements[0].Args[0].IsCallArg)
	assert.NotNil(t, module.Program.Statements[0].Args[1])
	assert.Equal(t, "A", module.Program.Statements[0].Args[1].Name)
	assert.Equal(t, 0, module.Program.Statements[0].Args[1].Index)
	assert.False(t, module.Program.Statements[0].Args[1].IsCallArg)

	assert.Equal(t, StmtLetEq, module.Program.Statements[1].Type)
	assert.Equal(t, 3, module.Program.Statements[1].LineNumber)
	assert.Len(t, module.Program.Statements[1].Args, 2)
	assert.NotNil(t, module.Program.Statements[1].Args[0])
	assert.Equal(t, 0, module.Program.Statements[1].Args[0].Index)
	assert.False(t, module.Program.Statements[1].Args[0].IsCallArg)
	assert.Nil(t, module.Program.Statements[1].Args[1])

	assert.Equal(t, StmtLetAddEdge, module.Program.Statements[2].Type)
	assert.Equal(t, 4, module.Program.Statements[2].LineNumber)
	assert.Len(t, module.Program.Statements[2].Args, 2)
	assert.NotNil(t, module.Program.Statements[2].Args[0])
	assert.Equal(t, "A", module.Program.Statements[2].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[2].Args[0].Index)
	assert.False(t, module.Program.Statements[2].Args[0].IsCallArg)
	assert.NotNil(t, module.Program.Statements[2].Args[1])
	assert.Equal(t, "A", module.Program.Statements[2].Args[1].Name)
	assert.Equal(t, 0, module.Program.Statements[2].Args[1].Index)
	assert.False(t, module.Program.Statements[2].Args[1].IsCallArg)

	assert.Equal(t, StmtLetAddEdge, module.Program.Statements[3].Type)
	assert.Equal(t, 5, module.Program.Statements[3].LineNumber)
	assert.Len(t, module.Program.Statements[3].Args, 2)
	assert.NotNil(t, module.Program.Statements[3].Args[0])
	assert.Equal(t, "A", module.Program.Statements[3].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[3].Args[0].Index)
	assert.False(t, module.Program.Statements[3].Args[0].IsCallArg)
	assert.Nil(t, module.Program.Statements[3].Args[1])

	assert.Equal(t, StmtLetRemoveEdge, module.Program.Statements[4].Type)
	assert.Equal(t, 6, module.Program.Statements[4].LineNumber)
	assert.Len(t, module.Program.Statements[4].Args, 2)
	assert.NotNil(t, module.Program.Statements[4].Args[0])
	assert.Equal(t, "A", module.Program.Statements[4].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[4].Args[0].Index)
	assert.False(t, module.Program.Statements[4].Args[0].IsCallArg)
	assert.NotNil(t, module.Program.Statements[4].Args[1])
	assert.Equal(t, "A", module.Program.Statements[4].Args[1].Name)
	assert.Equal(t, 0, module.Program.Statements[4].Args[1].Index)
	assert.False(t, module.Program.Statements[4].Args[1].IsCallArg)
}

func TestParseIf(t *testing.T) {
	module := testParse(t, `PROGRAM T
IF A = A
END IF
IF A > A
ELSE
END IF
END T`)

	assert.Equal(t, "T", module.Name)
	assert.Len(t, module.Use, 0)
	assert.Len(t, module.Subroutine, 0)
	assert.NotNil(t, module.Program)
	assert.Equal(t, "T", module.Program.Name)
	assert.Equal(t, 1, module.Program.LineNumber)
	assert.Equal(t, 7, module.Program.EndLineNumber)
	assert.False(t, module.Program.Exported)
	assert.Equal(t, 0, module.Program.ParameterCount)
	assert.Len(t, module.Program.Vars, 1)
	assert.Equal(t, 0, module.Program.DoEdgesCount)
	assert.Equal(t, 0, module.Program.CallArgsMaxCount)
	assert.Len(t, module.Program.Statements, 2)

	assert.Equal(t, "A", module.Program.Vars[0].Name)
	assert.Equal(t, 0, module.Program.Vars[0].Index)
	assert.False(t, module.Program.Vars[0].IsCallArg)

	assert.Equal(t, StmtIf, module.Program.Statements[0].Type)
	assert.Equal(t, 2, module.Program.Statements[0].LineNumber)
	assert.Equal(t, 3, module.Program.Statements[0].EndLineNumber)
	assert.Len(t, module.Program.Statements[0].IfBranches, 1)

	assert.Equal(t, IfBranchEq, module.Program.Statements[0].IfBranches[0].Type)
	assert.Equal(t, 2, module.Program.Statements[0].IfBranches[0].LineNumber)
	assert.Len(t, module.Program.Statements[0].IfBranches[0].Args, 2)
	assert.Len(t, module.Program.Statements[0].IfBranches[0].Statements, 0)
	assert.NotNil(t, module.Program.Statements[0].IfBranches[0].Args[0])
	assert.Equal(t, "A", module.Program.Statements[0].IfBranches[0].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[0].IfBranches[0].Args[0].Index)
	assert.False(t, module.Program.Statements[0].IfBranches[0].Args[0].IsCallArg)
	assert.NotNil(t, module.Program.Statements[0].IfBranches[0].Args[1])
	assert.Equal(t, "A", module.Program.Statements[0].IfBranches[0].Args[1].Name)
	assert.Equal(t, 0, module.Program.Statements[0].IfBranches[0].Args[1].Index)
	assert.False(t, module.Program.Statements[0].IfBranches[0].Args[1].IsCallArg)

	assert.Equal(t, StmtIf, module.Program.Statements[1].Type)
	assert.Equal(t, 4, module.Program.Statements[1].LineNumber)
	assert.Equal(t, 6, module.Program.Statements[1].EndLineNumber)
	assert.Len(t, module.Program.Statements[1].IfBranches, 2)

	assert.Equal(t, IfBranchEdge, module.Program.Statements[1].IfBranches[0].Type)
	assert.Equal(t, 4, module.Program.Statements[1].IfBranches[0].LineNumber)
	assert.Len(t, module.Program.Statements[1].IfBranches[0].Args, 2)
	assert.Len(t, module.Program.Statements[1].IfBranches[0].Statements, 0)
	assert.NotNil(t, module.Program.Statements[1].IfBranches[0].Args[0])
	assert.Equal(t, "A", module.Program.Statements[1].IfBranches[0].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[1].IfBranches[0].Args[0].Index)
	assert.False(t, module.Program.Statements[1].IfBranches[0].Args[0].IsCallArg)
	assert.NotNil(t, module.Program.Statements[1].IfBranches[0].Args[1])
	assert.Equal(t, "A", module.Program.Statements[1].IfBranches[0].Args[1].Name)
	assert.Equal(t, 0, module.Program.Statements[1].IfBranches[0].Args[1].Index)
	assert.False(t, module.Program.Statements[1].IfBranches[0].Args[1].IsCallArg)

	assert.Equal(t, IfBranchElse, module.Program.Statements[1].IfBranches[1].Type)
	assert.Equal(t, 5, module.Program.Statements[1].IfBranches[1].LineNumber)
	assert.Len(t, module.Program.Statements[1].IfBranches[1].Args, 0)
	assert.Len(t, module.Program.Statements[1].IfBranches[1].Statements, 0)
}

func TestParseCall(t *testing.T) {
	module := testParse(t, `USE M
SUBROUTINE A()
END A
PROGRAM T
  CALL A()
  CALL M.M()
END T`)
	assert.Equal(t, "T", module.Name)
	assert.Len(t, module.Use, 1)
	assert.Equal(t, 1, module.Use["M"])
	assert.Len(t, module.Subroutine, 1)
	assert.NotNil(t, module.Program)
	assert.Equal(t, "T", module.Program.Name)
	assert.Equal(t, 4, module.Program.LineNumber)
	assert.Equal(t, 7, module.Program.EndLineNumber)
	assert.False(t, module.Program.Exported)
	assert.Equal(t, 0, module.Program.ParameterCount)
	assert.Len(t, module.Program.Vars, 0)
	assert.Equal(t, 0, module.Program.DoEdgesCount)
	assert.Equal(t, 0, module.Program.CallArgsMaxCount)
	assert.Len(t, module.Program.Statements, 2)

	a, ok := module.Subroutine["A"]
	assert.True(t, ok)
	assert.Equal(t, "A", a.Name)
	assert.Equal(t, 2, a.LineNumber)
	assert.Equal(t, 3, a.EndLineNumber)
	assert.False(t, a.Exported)
	assert.Equal(t, 0, a.ParameterCount)
	assert.Len(t, a.Vars, 0)
	assert.Equal(t, 0, a.DoEdgesCount)
	assert.Equal(t, 0, a.CallArgsMaxCount)
	assert.Len(t, a.Statements, 0)

	assert.Equal(t, StmtCall, module.Program.Statements[0].Type)
	assert.Equal(t, 5, module.Program.Statements[0].LineNumber)
	assert.Len(t, module.Program.Statements[0].Args, 0)
	assert.Equal(t, "", module.Program.Statements[0].CallTargetModule)
	assert.Equal(t, "A", module.Program.Statements[0].CallTargetRoutine)

	assert.Equal(t, StmtCall, module.Program.Statements[1].Type)
	assert.Equal(t, 6, module.Program.Statements[1].LineNumber)
	assert.Len(t, module.Program.Statements[1].Args, 0)
	assert.Equal(t, "M", module.Program.Statements[1].CallTargetModule)
	assert.Equal(t, "M", module.Program.Statements[1].CallTargetRoutine)

	module = testParse(t, `USE M
PROGRAM T
  CALL M.M(A,0,B)
END T`)
	assert.Equal(t, "T", module.Name)
	assert.Len(t, module.Use, 1)
	assert.Equal(t, 1, module.Use["M"])
	assert.Len(t, module.Subroutine, 0)
	assert.NotNil(t, module.Program)
	assert.Equal(t, "T", module.Program.Name)
	assert.Equal(t, 2, module.Program.LineNumber)
	assert.Equal(t, 4, module.Program.EndLineNumber)
	assert.False(t, module.Program.Exported)
	assert.Equal(t, 0, module.Program.ParameterCount)
	assert.Len(t, module.Program.Vars, 2)
	assert.Equal(t, 0, module.Program.DoEdgesCount)
	assert.Equal(t, 3, module.Program.CallArgsMaxCount)
	assert.Len(t, module.Program.Statements, 1)

	assert.Equal(t, "A", module.Program.Vars[0].Name)
	assert.Equal(t, 0, module.Program.Vars[0].Index)
	assert.False(t, module.Program.Vars[0].IsCallArg)

	assert.Equal(t, "B", module.Program.Vars[1].Name)
	assert.Equal(t, 1, module.Program.Vars[1].Index)
	assert.False(t, module.Program.Vars[1].IsCallArg)

	assert.Equal(t, StmtCall, module.Program.Statements[0].Type)
	assert.Equal(t, 3, module.Program.Statements[0].LineNumber)
	assert.Len(t, module.Program.Statements[0].Args, 3)
	assert.Equal(t, "M", module.Program.Statements[0].CallTargetModule)
	assert.Equal(t, "M", module.Program.Statements[0].CallTargetRoutine)

	assert.NotNil(t, module.Program.Statements[0].Args[0])
	assert.Equal(t, "A", module.Program.Statements[0].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[0].Args[0].Index)
	assert.False(t, module.Program.Statements[0].Args[0].IsCallArg)

	assert.Nil(t, module.Program.Statements[0].Args[1])

	assert.NotNil(t, module.Program.Statements[0].Args[2])
	assert.Equal(t, "B", module.Program.Statements[0].Args[2].Name)
	assert.Equal(t, 1, module.Program.Statements[0].Args[2].Index)
	assert.False(t, module.Program.Statements[0].Args[2].IsCallArg)
}

func TestParseReturn(t *testing.T) {
	module := testParse(t, `SUBROUTINE A()
RETURN
END A
PROGRAM T
END T`)
	assert.Equal(t, "T", module.Name)
	assert.Len(t, module.Use, 0)
	assert.Len(t, module.Subroutine, 1)
	assert.NotNil(t, module.Program)
	assert.Equal(t, "T", module.Program.Name)
	assert.Equal(t, 4, module.Program.LineNumber)
	assert.Equal(t, 5, module.Program.EndLineNumber)
	assert.False(t, module.Program.Exported)
	assert.Equal(t, 0, module.Program.ParameterCount)
	assert.Len(t, module.Program.Vars, 0)
	assert.Equal(t, 0, module.Program.DoEdgesCount)
	assert.Equal(t, 0, module.Program.CallArgsMaxCount)
	assert.Len(t, module.Program.Statements, 0)

	a, ok := module.Subroutine["A"]
	assert.True(t, ok)
	assert.Equal(t, "A", a.Name)
	assert.Equal(t, 1, a.LineNumber)
	assert.Equal(t, 3, a.EndLineNumber)
	assert.False(t, a.Exported)
	assert.Equal(t, 0, a.ParameterCount)
	assert.Len(t, a.Vars, 0)
	assert.Equal(t, 0, a.DoEdgesCount)
	assert.Equal(t, 0, a.CallArgsMaxCount)
	assert.Len(t, a.Statements, 1)

	assert.Equal(t, StmtReturn, a.Statements[0].Type)
	assert.Equal(t, 2, a.Statements[0].LineNumber)
}

func TestParseDo(t *testing.T) {
	module := testParse(t, `PROGRAM T
DO A < A
  DO A
  END DO
END DO
DO A < A
  DO A < A
  END DO
END DO
END T`)
	assert.Equal(t, "T", module.Name)
	assert.Len(t, module.Use, 0)
	assert.Len(t, module.Subroutine, 0)
	assert.NotNil(t, module.Program)
	assert.Equal(t, "T", module.Program.Name)
	assert.Equal(t, 1, module.Program.LineNumber)
	assert.Equal(t, 10, module.Program.EndLineNumber)
	assert.False(t, module.Program.Exported)
	assert.Equal(t, 0, module.Program.ParameterCount)
	assert.Len(t, module.Program.Vars, 1)
	assert.Equal(t, 3, module.Program.DoEdgesCount)
	assert.Equal(t, 0, module.Program.CallArgsMaxCount)
	assert.Len(t, module.Program.Statements, 2)

	assert.Equal(t, "A", module.Program.Vars[0].Name)
	assert.Equal(t, 0, module.Program.Vars[0].Index)
	assert.False(t, module.Program.Vars[0].IsCallArg)

	assert.Equal(t, StmtDoEdges, module.Program.Statements[0].Type)
	assert.Equal(t, 2, module.Program.Statements[0].LineNumber)
	assert.Equal(t, 5, module.Program.Statements[0].EndLineNumber)
	assert.Len(t, module.Program.Statements[0].Args, 2)
	assert.Len(t, module.Program.Statements[0].Statements, 1)
	assert.Equal(t, 0, module.Program.Statements[0].DoLoopIndex)
	assert.Equal(t, 0, module.Program.Statements[0].DoEdgesIndex)
	assert.NotNil(t, module.Program.Statements[0].Args[0])
	assert.Equal(t, "A", module.Program.Statements[0].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[0].Args[0].Index)
	assert.False(t, module.Program.Statements[0].Args[0].IsCallArg)
	assert.NotNil(t, module.Program.Statements[0].Args[1])
	assert.Equal(t, "A", module.Program.Statements[0].Args[1].Name)
	assert.Equal(t, 0, module.Program.Statements[0].Args[1].Index)
	assert.False(t, module.Program.Statements[0].Args[1].IsCallArg)

	assert.Equal(t, StmtDoLoop, module.Program.Statements[0].Statements[0].Type)
	assert.Equal(t, 3, module.Program.Statements[0].Statements[0].LineNumber)
	assert.Equal(t, 4, module.Program.Statements[0].Statements[0].EndLineNumber)
	assert.Len(t, module.Program.Statements[0].Statements[0].Args, 1)
	assert.Len(t, module.Program.Statements[0].Statements[0].Statements, 0)
	assert.Equal(t, 1, module.Program.Statements[0].Statements[0].DoLoopIndex)
	assert.NotNil(t, module.Program.Statements[0].Statements[0].Args[0])
	assert.Equal(t, "A", module.Program.Statements[0].Statements[0].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[0].Statements[0].Args[0].Index)
	assert.False(t, module.Program.Statements[0].Statements[0].Args[0].IsCallArg)

	assert.Equal(t, StmtDoEdges, module.Program.Statements[1].Type)
	assert.Equal(t, 6, module.Program.Statements[1].LineNumber)
	assert.Equal(t, 9, module.Program.Statements[1].EndLineNumber)
	assert.Len(t, module.Program.Statements[1].Args, 2)
	assert.Len(t, module.Program.Statements[1].Statements, 1)
	assert.Equal(t, 2, module.Program.Statements[1].DoLoopIndex)
	assert.Equal(t, 1, module.Program.Statements[1].DoEdgesIndex)
	assert.NotNil(t, module.Program.Statements[1].Args[0])
	assert.Equal(t, "A", module.Program.Statements[1].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[1].Args[0].Index)
	assert.False(t, module.Program.Statements[1].Args[0].IsCallArg)
	assert.NotNil(t, module.Program.Statements[1].Args[1])
	assert.Equal(t, "A", module.Program.Statements[1].Args[1].Name)
	assert.Equal(t, 0, module.Program.Statements[1].Args[1].Index)
	assert.False(t, module.Program.Statements[1].Args[1].IsCallArg)

	assert.Equal(t, StmtDoEdges, module.Program.Statements[1].Statements[0].Type)
	assert.Equal(t, 7, module.Program.Statements[1].Statements[0].LineNumber)
	assert.Equal(t, 8, module.Program.Statements[1].Statements[0].EndLineNumber)
	assert.Len(t, module.Program.Statements[1].Statements[0].Args, 2)
	assert.Len(t, module.Program.Statements[1].Statements[0].Statements, 0)
	assert.Equal(t, 3, module.Program.Statements[1].Statements[0].DoLoopIndex)
	assert.Equal(t, 2, module.Program.Statements[1].Statements[0].DoEdgesIndex)
	assert.NotNil(t, module.Program.Statements[1].Statements[0].Args[0])
	assert.Equal(t, "A", module.Program.Statements[1].Statements[0].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[1].Statements[0].Args[0].Index)
	assert.False(t, module.Program.Statements[1].Statements[0].Args[0].IsCallArg)
	assert.NotNil(t, module.Program.Statements[1].Statements[0].Args[1])
	assert.Equal(t, "A", module.Program.Statements[1].Statements[0].Args[1].Name)
	assert.Equal(t, 0, module.Program.Statements[1].Statements[0].Args[1].Index)
	assert.False(t, module.Program.Statements[1].Statements[0].Args[1].IsCallArg)
}

func TestParseExit(t *testing.T) {
	module := testParse(t, `PROGRAM T
DO A
  DO B
    IF A = B
      EXIT A
    ELSE
      EXIT B
    END IF
  END DO
END DO
END T`)
	assert.Equal(t, "T", module.Name)
	assert.Len(t, module.Use, 0)
	assert.Len(t, module.Subroutine, 0)
	assert.NotNil(t, module.Program)
	assert.Equal(t, "T", module.Program.Name)
	assert.Equal(t, 1, module.Program.LineNumber)
	assert.Equal(t, 11, module.Program.EndLineNumber)
	assert.False(t, module.Program.Exported)
	assert.Equal(t, 0, module.Program.ParameterCount)
	assert.Len(t, module.Program.Vars, 2)
	assert.Equal(t, 0, module.Program.DoEdgesCount)
	assert.Equal(t, 0, module.Program.CallArgsMaxCount)
	assert.Len(t, module.Program.Statements, 1)

	assert.Equal(t, "A", module.Program.Vars[0].Name)
	assert.Equal(t, 0, module.Program.Vars[0].Index)
	assert.False(t, module.Program.Vars[0].IsCallArg)

	assert.Equal(t, "B", module.Program.Vars[1].Name)
	assert.Equal(t, 1, module.Program.Vars[1].Index)
	assert.False(t, module.Program.Vars[1].IsCallArg)

	assert.Equal(t, StmtDoLoop, module.Program.Statements[0].Type)
	assert.Equal(t, 2, module.Program.Statements[0].LineNumber)
	assert.Equal(t, 10, module.Program.Statements[0].EndLineNumber)
	assert.Len(t, module.Program.Statements[0].Args, 1)
	assert.Len(t, module.Program.Statements[0].Statements, 1)
	assert.Equal(t, 0, module.Program.Statements[0].DoLoopIndex)

	assert.NotNil(t, module.Program.Statements[0].Args[0])
	assert.Equal(t, "A", module.Program.Statements[0].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[0].Args[0].Index)
	assert.False(t, module.Program.Statements[0].Args[0].IsCallArg)

	assert.Equal(t, StmtDoLoop, module.Program.Statements[0].Statements[0].Type)
	assert.Equal(t, 3, module.Program.Statements[0].Statements[0].LineNumber)
	assert.Equal(t, 9, module.Program.Statements[0].Statements[0].EndLineNumber)
	assert.Len(t, module.Program.Statements[0].Statements[0].Args, 1)
	assert.Len(t, module.Program.Statements[0].Statements[0].Statements, 1)
	assert.Equal(t, 1, module.Program.Statements[0].Statements[0].DoLoopIndex)

	assert.NotNil(t, module.Program.Statements[0].Statements[0].Args[0])
	assert.Equal(t, "B", module.Program.Statements[0].Statements[0].Args[0].Name)
	assert.Equal(t, 1, module.Program.Statements[0].Statements[0].Args[0].Index)
	assert.False(t, module.Program.Statements[0].Statements[0].Args[0].IsCallArg)

	assert.Equal(t, StmtIf, module.Program.Statements[0].Statements[0].Statements[0].Type)
	assert.Equal(t, 4, module.Program.Statements[0].Statements[0].Statements[0].LineNumber)
	assert.Equal(t, 8, module.Program.Statements[0].Statements[0].Statements[0].EndLineNumber)
	assert.Len(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches, 2)

	assert.Equal(t, IfBranchEq, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Type)
	assert.Equal(t, 4, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].LineNumber)
	assert.Len(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Args, 2)
	assert.Len(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Statements, 1)

	assert.Equal(t, IfBranchElse, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].Type)
	assert.Equal(t, 6, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].LineNumber)
	assert.Len(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].Args, 0)
	assert.Len(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].Statements, 1)

	assert.NotNil(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Args[0])
	assert.Equal(t, "A", module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Args[0].Index)
	assert.False(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Args[0].IsCallArg)

	assert.NotNil(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Args[1])
	assert.Equal(t, "B", module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Args[1].Name)
	assert.Equal(t, 1, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Args[1].Index)
	assert.False(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Args[1].IsCallArg)

	assert.Equal(t, StmtExit, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Statements[0].Type)
	assert.Equal(t, 5, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Statements[0].LineNumber)
	assert.Len(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Statements[0].Args, 1)
	assert.Equal(t, 0, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Statements[0].DoLoopIndex)
	assert.NotNil(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Statements[0].Args[0])
	assert.Equal(t, "A", module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Statements[0].Args[0].Name)
	assert.Equal(t, 0, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Statements[0].Args[0].Index)
	assert.False(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[0].Statements[0].Args[0].IsCallArg)

	assert.Equal(t, StmtExit, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].Statements[0].Type)
	assert.Equal(t, 7, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].Statements[0].LineNumber)
	assert.Len(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].Statements[0].Args, 1)
	assert.Equal(t, 1, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].Statements[0].DoLoopIndex)
	assert.NotNil(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].Statements[0].Args[0])
	assert.Equal(t, "B", module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].Statements[0].Args[0].Name)
	assert.Equal(t, 1, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].Statements[0].Args[0].Index)
	assert.False(t, module.Program.Statements[0].Statements[0].Statements[0].IfBranches[1].Statements[0].Args[0].IsCallArg)
}

func TestParseErrors(t *testing.T) {
	testParseError(t, ``, "EOF")
	testParseError(t, `USE .`, "<test>:1: SYNTAX ERROR")
	testParseError(t, `USE T
USE T`, "<test>:2: DUPLICATE USE")
	testParseError(t, `SUBROUTINE T()`, "<test>:1: SYNTAX ERROR")
	testParseError(t, `SUBROUTINE .()`, "<test>:1: SYNTAX ERROR")
	testParseError(t, `SUBROUTINE T()
END T
SUBROUTINE T()
END T`, "<test>:3: DUPLICATE SUBROUTINE NAME")
	testParseError(t, `SUBROUTINE T(,)`, "<test>:1: SYNTAX ERROR")
	testParseError(t, `SUBROUTINE T(A,A)`, "<test>:1: SYNTAX ERROR")
	testParseError(t, `LIBRARY T`, "<test>:1: SYNTAX ERROR")
	testParseError(t, `LIBRARY T
END T`, "<test>:2: EMPTY LIBRARY")
	testParseError(t, `LIBRARY .`, "<test>:1: SYNTAX ERROR")
	testParseError(t, `LIBRARY T
SUBROUTINE T
END T`, "<test>:2: UNDEFINED SUBROUTINE")
	testParseError(t, `SUBROUTINE T()
END T
LIBRARY T
SUBROUTINE T
SUBROUTINE T
END T`, "<test>:5: DUPLICATE SUBROUTINE")
	testParseError(t, `LIBRARY T
SUBROUTINE .
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T`, "<test>:1: SYNTAX ERROR")
	testParseError(t, `PROGRAM .`, "<test>:1: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
END T
RETURN`, "<test>:3: TRAILING JUNK")
	testParseError(t, `PROGRAM T
LET
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
LET 0 = 0
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
LET A . 0
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
LET A < 0
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
IF
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
IF A = A
END DO
END T`, "<test>:3: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
CALL
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
CALL M.M()
END T`, "<test>:2: UNDECLARED MODULE")
	testParseError(t, `PROGRAM T
CALL =()
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
RETURN
END T`, "<test>:2: ILLEGAL RETURN")
	testParseError(t, `PROGRAM T
RETURNT
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
DO
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
DO A
END IF
END T`, "<test>:3: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
EXIT
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
EXIT.
END T`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
EXITT
END T`, "<test>:2: INVALID EXIT")
	testParseError(t, `PROGRAM T
IF 0 = 0`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
IF A = 0`, "<test>:2: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
IF A = A
ELSE D0`, "<test>:3: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
IF A = A
ELSE
ELSE`, "<test>:4: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
IF A = A
  CALL A.A()
END IF
END T`, "<test>:3: UNDECLARED MODULE")
	testParseError(t, `SUBROUTINE T()
IF A = A
  CALL A.A()`, "<test>:3: UNDECLARED MODULE")
	testParseError(t, `SUBROUTINE T()
IF A = A
ELSE
  CALL A.A()`, "<test>:4: UNDECLARED MODULE")
	testParseError(t, `PROGRAM T
DO A
  CALL A.A()`, "<test>:3: UNDECLARED MODULE")
	testParseError(t, `SUBROUTINE A(A,)`, "<test>:1: SYNTAX ERROR")
	testParseError(t, `SUBROUTINE A(0)`, "<test>:1: SYNTAX ERROR")
	testParseError(t, `PROGRAM T
DO A
  RETURN
END DO
END T`, "<test>:3: ILLEGAL RETURN")
}
