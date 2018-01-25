package main

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"strings"
	"unicode"
)

func Parse(filename, dir string, r io.Reader) (*ASTModule, error) {
	astModule := &ASTModule{
		Filename:   filename,
		Dir:        dir,
		Use:        make(map[string]int),
		Subroutine: make(map[string]ASTRoutine),
	}
	t := newTokenizer(filename, r)
	if err := t.nextLine(); err != nil {
		return nil, err
	}
	if err := parseUses(t, astModule); err != nil {
		return nil, err
	}
	if err := parseSubroutines(t, astModule); err != nil {
		return nil, err
	}
	if hasLib, err := parseLibrary(t, astModule); err != nil {
		return nil, err
	} else if !hasLib {
		if err := parseProgram(t, astModule); err != nil {
			return nil, err
		}
	}
	if !t.atEOF && t.nextLine() != io.EOF {
		return nil, t.makeErr("TRAILING JUNK")
	}
	for _, subroutine := range astModule.Subroutine {
		if err := checkCalls(astModule, subroutine.Statements); err != nil {
			return nil, err
		}
	}
	if astModule.Program != nil {
		if err := checkCalls(astModule, astModule.Program.Statements); err != nil {
			return nil, err
		}
	}
	return astModule, nil
}

type tokenizer struct {
	filename   string
	r          *bufio.Reader
	lineNumber int
	tokens     []string
	atEOF      bool
}

func newTokenizer(filename string, r io.Reader) *tokenizer {
	return &tokenizer{filename: filename, r: bufio.NewReader(r)}
}

func (t *tokenizer) nextLine() error {
	if t.atEOF {
		return io.EOF
	}
	t.tokens = t.tokens[:0]
	var buffer bytes.Buffer
	for {
		t.lineNumber++
		inComment := false
		for {
			ch, _, err := t.r.ReadRune()
			if err == io.EOF {
				t.atEOF = true
				if buffer.Len() > 0 {
					t.tokens = append(t.tokens, buffer.String())
					buffer.Reset()
				}
				if len(t.tokens) == 0 {
					return err
				} else {
					return nil
				}
			} else if err != nil {
				return err
			} else if ch == '\n' {
				if buffer.Len() > 0 {
					t.tokens = append(t.tokens, buffer.String())
					buffer.Reset()
				}
				if len(t.tokens) == 0 {
					break
				} else {
					return nil
				}
			} else if inComment {
			} else if ch == '*' {
				if buffer.Len() > 0 {
					t.tokens = append(t.tokens, buffer.String())
					buffer.Reset()
				}
				inComment = true
			} else if unicode.IsSpace(ch) {
			} else if unicode.IsLetter(ch) || unicode.IsNumber(ch) {
				buffer.WriteRune(ch)
			} else {
				if buffer.Len() > 0 {
					t.tokens = append(t.tokens, buffer.String())
					buffer.Reset()
				}
				t.tokens = append(t.tokens, string(ch))
			}
		}
	}
}

func (t *tokenizer) tokenize(keyword string) (int, []string) {
	if len(t.tokens) == 0 || !strings.HasPrefix(t.tokens[0], keyword) {
		return t.lineNumber, nil
	}
	t.tokens[0] = t.tokens[0][len(keyword):]
	return t.lineNumber, t.tokens
}

func (t *tokenizer) makeErr(message string) error {
	return errors.New(fmt.Sprintf("%s:%d: %s", t.filename, t.lineNumber, message))
}

func (t *tokenizer) makeErrAt(lineNumber int, message string) error {
	return errors.New(fmt.Sprintf("%s:%d: %s", t.filename, lineNumber, message))
}

func parseUses(t *tokenizer, astModule *ASTModule) error {
	for {
		lineNumber, tokens := t.tokenize("USE")
		if tokens == nil {
			return nil
		}
		if len(tokens) != 1 || !isIdent(tokens[0]) {
			return t.makeErr("SYNTAX ERROR")
		} else if _, ok := astModule.Use[tokens[0]]; ok {
			return t.makeErr("DUPLICATE USE")
		}
		astModule.Use[tokens[0]] = lineNumber
		t.nextLine()
	}
}

func parseSubroutines(t *tokenizer, astModule *ASTModule) error {
	for {
		lineNumber, tokens := t.tokenize("SUBROUTINE")
		if len(tokens) < 3 {
			return nil
		}
		name := tokens[0]
		if !isIdent(name) {
			return t.makeErr("SYNTAX ERROR")
		}
		if _, ok := astModule.Subroutine[name]; ok {
			return t.makeErr("DUPLICATE SUBROUTINE NAME")
		}
		args, ok := parseArgs(tokens[1:])
		if !ok {
			return t.makeErr("SYNTAX ERROR")
		}
		routineInfo := makeRoutineInfo(args, astModule.Use)
		if routineInfo == nil {
			return t.makeErr("SYNTAX ERROR")
		}
		statements, err := parseStatements(t, routineInfo)
		if err != nil {
			return err
		}
		endLineNumber, tokens := t.tokenize("END")
		if len(tokens) != 1 || tokens[0] != name {
			return t.makeErr("SYNTAX ERROR")
		}
		astModule.Subroutine[name] = ASTRoutine{
			Name:             name,
			LineNumber:       lineNumber,
			EndLineNumber:    endLineNumber,
			Exported:         false,
			ParameterCount:   len(args),
			Vars:             routineInfo.varList,
			DoEdgesCount:     routineInfo.doEdgesCount,
			CallArgsMaxCount: routineInfo.callArgsMaxCount,
			Statements:       statements,
		}
		t.nextLine()
	}
}

func parseLibrary(t *tokenizer, astModule *ASTModule) (bool, error) {
	_, tokens := t.tokenize("LIBRARY")
	if tokens == nil {
		return false, nil
	} else if len(tokens) != 1 || !isIdent(tokens[0]) {
		return false, t.makeErr("SYNTAX ERROR")
	}
	astModule.Name = tokens[0]
	exportCount := 0
	for {
		t.nextLine()
		_, tokens := t.tokenize("SUBROUTINE")
		if tokens == nil {
			break
		} else if len(tokens) != 1 || !isIdent(tokens[0]) {
			return false, t.makeErr("SYNTAX ERROR")
		}
		subroutine, ok := astModule.Subroutine[tokens[0]]
		if !ok {
			return false, t.makeErr("UNDEFINED SUBROUTINE")
		} else if subroutine.Exported {
			return false, t.makeErr("DUPLICATE SUBROUTINE")
		}
		subroutine.Exported = true
		astModule.Subroutine[tokens[0]] = subroutine
		exportCount++
	}
	_, tokens = t.tokenize("END")
	if len(tokens) != 1 || tokens[0] != astModule.Name {
		return false, t.makeErr("SYNTAX ERROR")
	}
	if exportCount == 0 {
		return false, t.makeErr("EMPTY LIBRARY")
	}
	return true, nil
}

func parseProgram(t *tokenizer, astModule *ASTModule) error {
	lineNumber, tokens := t.tokenize("PROGRAM")
	if len(tokens) != 1 || !isIdent(tokens[0]) {
		return t.makeErr("SYNTAX ERROR")
	}
	astModule.Name = tokens[0]
	routineInfo := makeRoutineInfo([]string{}, astModule.Use)
	statements, err := parseStatements(t, routineInfo)
	if err != nil {
		return err
	}
	endLineNumber, tokens := t.tokenize("END")
	if len(tokens) != 1 || tokens[0] != astModule.Name {
		return t.makeErr("SYNTAX ERROR")
	}
	if retLineNumber, hasRet := hasReturn(statements); hasRet {
		return t.makeErrAt(retLineNumber, "ILLEGAL RETURN")
	}
	astModule.Program = &ASTRoutine{
		Name:             astModule.Name,
		LineNumber:       lineNumber,
		EndLineNumber:    endLineNumber,
		Vars:             routineInfo.varList,
		DoEdgesCount:     routineInfo.doEdgesCount,
		CallArgsMaxCount: routineInfo.callArgsMaxCount,
		Statements:       statements,
	}
	return nil
}

func parseStatements(t *tokenizer, routineInfo *routineInfo) ([]ASTStatement, error) {
	var statements []ASTStatement
	for {
		t.nextLine()
		statement, ok, err := parseStatement(t, routineInfo)
		if err != nil {
			return nil, err
		}
		if !ok {
			return statements, nil
		}
		statements = append(statements, statement)
	}
}

func parseStatement(t *tokenizer, routineInfo *routineInfo) (ASTStatement, bool, error) {
	if lineNumber, tokens := t.tokenize("LET"); tokens != nil {
		if len(tokens) != 3 {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		var0, ok := routineInfo.getVar(tokens[0])
		if !ok {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		var stmtType ASTStatementType
		var var1 *ASTVar
		if tokens[1] == "=" {
			stmtType = StmtLetEq
			var1, ok = routineInfo.getVal(tokens[2])
		} else if tokens[1] == ">" {
			stmtType = StmtLetAddEdge
			var1, ok = routineInfo.getVal(tokens[2])
		} else if tokens[1] == "<" {
			stmtType = StmtLetRemoveEdge
			var1var, ok1 := routineInfo.getVar(tokens[2])
			var1, ok = &var1var, ok1
		} else {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		if !ok {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		return ASTStatement{
			Type:       stmtType,
			LineNumber: lineNumber,
			Args:       []*ASTVar{&var0, var1},
		}, true, nil
	} else if lineNumber, tokens := t.tokenize("IF"); tokens != nil {
		t.tokens[0] = "ELSEIF" + t.tokens[0]
		ifBranches, err := parseIfBranches(t, routineInfo)
		if err != nil {
			return ASTStatement{}, false, err
		}
		endLineNumber, tokens := t.tokenize("END")
		if len(tokens) != 1 || tokens[0] != "IF" {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		return ASTStatement{
			Type:          StmtIf,
			LineNumber:    lineNumber,
			EndLineNumber: endLineNumber,
			IfBranches:    ifBranches,
		}, true, nil
	} else if lineNumber, tokens := t.tokenize("CALL"); tokens != nil {
		if len(tokens) < 3 {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		modName := ""
		routName := tokens[0]
		var args []string
		var ok bool
		if tokens[1] == "." {
			modName = tokens[0]
			routName = tokens[2]
			args, ok = parseArgs(tokens[3:])
			if _, ok := routineInfo.uses[modName]; !ok {
				return ASTStatement{}, false, t.makeErr("UNDECLARED MODULE")
			}
		} else {
			args, ok = parseArgs(tokens[1:])
		}
		if !ok || !isIdent(routName) || (modName != "" && !isIdent(modName)) {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		stmt := ASTStatement{
			Type:              StmtCall,
			LineNumber:        lineNumber,
			CallTargetModule:  modName,
			CallTargetRoutine: routName,
		}
		for _, arg := range args {
			val, ok := routineInfo.getVal(arg)
			if !ok {
				return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
			}
			stmt.Args = append(stmt.Args, val)
		}
		if routineInfo.callArgsMaxCount < len(args) {
			routineInfo.callArgsMaxCount = len(args)
		}
		return stmt, true, nil
	} else if lineNumber, tokens := t.tokenize("RETURN"); tokens != nil {
		if len(tokens) != 1 || tokens[0] != "" {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		return ASTStatement{
			Type:       StmtReturn,
			LineNumber: lineNumber,
		}, true, nil
	} else if lineNumber, tokens := t.tokenize("DO"); tokens != nil {
		var stmtType ASTStatementType
		var args []*ASTVar
		ok := false
		doLoopIndex := routineInfo.doLoopCount
		routineInfo.doLoopCount++
		doEdgesIndex := 0
		if len(tokens) == 1 {
			stmtType = StmtDoLoop
			var arg ASTVar
			arg, ok = routineInfo.getVar(tokens[0])
			args = append(args, &arg)
		} else if len(tokens) == 3 && tokens[1] == "<" {
			stmtType = StmtDoEdges
			var arg ASTVar
			arg, ok = routineInfo.getVar(tokens[0])
			args = append(args, &arg)
			if ok {
				arg1, ok1 := routineInfo.getVar(tokens[2])
				args = append(args, &arg1)
				ok = ok1
			}
			doEdgesIndex = routineInfo.doEdgesCount
			routineInfo.doEdgesCount++
		}
		if !ok {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		routineInfo.doStackLabel = append(routineInfo.doStackLabel, tokens[0])
		routineInfo.doStackIndex = append(routineInfo.doStackIndex, doLoopIndex)
		statements, err := parseStatements(t, routineInfo)
		if err != nil {
			return ASTStatement{}, false, err
		}
		routineInfo.doStackLabel = routineInfo.doStackLabel[0 : len(routineInfo.doStackLabel)-1]
		routineInfo.doStackIndex = routineInfo.doStackIndex[0 : len(routineInfo.doStackIndex)-1]
		endLineNumber, tokens := t.tokenize("END")
		if len(tokens) != 1 || tokens[0] != "DO" {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		return ASTStatement{
			Type:          stmtType,
			LineNumber:    lineNumber,
			EndLineNumber: endLineNumber,
			Args:          args,
			Statements:    statements,
			DoLoopIndex:   doLoopIndex,
			DoEdgesIndex:  doEdgesIndex,
		}, true, nil
	} else if lineNumber, tokens := t.tokenize("EXIT"); tokens != nil {
		if len(tokens) != 1 {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		arg, ok := routineInfo.getVar(tokens[0])
		if !ok {
			return ASTStatement{}, false, t.makeErr("SYNTAX ERROR")
		}
		for i := len(routineInfo.doStackLabel) - 1; i >= 0; i-- {
			if tokens[0] == routineInfo.doStackLabel[i] {
				return ASTStatement{
					Type:        StmtExit,
					LineNumber:  lineNumber,
					Args:        []*ASTVar{&arg},
					DoLoopIndex: routineInfo.doStackIndex[i],
				}, true, nil
			}
		}
		return ASTStatement{}, false, t.makeErr("INVALID EXIT")
	} else {
		return ASTStatement{}, false, nil
	}
}

func parseIfBranches(t *tokenizer, routineInfo *routineInfo) ([]ASTIfBranch, error) {
	var ifBranches []ASTIfBranch
	for {
		ifBranch, ok, err := parseIfBranch(t, routineInfo)
		if err != nil {
			return nil, err
		}
		if !ok {
			return ifBranches, nil
		}
		ifBranches = append(ifBranches, ifBranch)
		if ifBranch.Type == IfBranchElse {
			return ifBranches, nil
		}
	}
}

func parseIfBranch(t *tokenizer, routineInfo *routineInfo) (ASTIfBranch, bool, error) {
	if lineNumber, tokens := t.tokenize("ELSEIF"); tokens != nil {
		var ifBranchType ASTIfBranchType
		if len(tokens) == 3 && tokens[1] == "=" {
			ifBranchType = IfBranchEq
		} else if len(tokens) == 3 && tokens[1] == ">" {
			ifBranchType = IfBranchEdge
		} else {
			return ASTIfBranch{}, false, t.makeErr("SYNTAX ERROR")
		}
		arg0, ok := routineInfo.getVar(tokens[0])
		if !ok {
			return ASTIfBranch{}, false, t.makeErr("SYNTAX ERROR")
		}
		arg1, ok := routineInfo.getVar(tokens[2])
		if !ok {
			return ASTIfBranch{}, false, t.makeErr("SYNTAX ERROR")
		}
		statements, err := parseStatements(t, routineInfo)
		if err != nil {
			return ASTIfBranch{}, false, err
		}
		return ASTIfBranch{
			Type:       ifBranchType,
			LineNumber: lineNumber,
			Args:       []ASTVar{arg0, arg1},
			Statements: statements,
		}, true, nil
	} else if lineNumber, tokens := t.tokenize("ELSE"); tokens != nil {
		if len(tokens) != 1 || tokens[0] != "" {
			return ASTIfBranch{}, false, t.makeErr("SYNTAX ERROR")
		}
		statements, err := parseStatements(t, routineInfo)
		if err != nil {
			return ASTIfBranch{}, false, err
		}
		return ASTIfBranch{
			Type:       IfBranchElse,
			LineNumber: lineNumber,
			Statements: statements,
		}, true, nil
	} else {
		return ASTIfBranch{}, false, nil
	}
}

type routineInfo struct {
	varList          []ASTVar
	varMap           map[string]ASTVar
	doStackLabel     []string
	doStackIndex     []int
	doLoopCount      int
	doEdgesCount     int
	callArgsMaxCount int
	uses             map[string]int
}

func makeRoutineInfo(args []string, uses map[string]int) *routineInfo {
	r := &routineInfo{
		varMap: make(map[string]ASTVar),
		uses:   uses,
	}
	for _, arg := range args {
		if !isIdent(arg) {
			return nil
		}
		if _, ok := r.varMap[arg]; ok {
			return nil
		}
		v := ASTVar{
			Name:      arg,
			Index:     len(r.varList),
			IsCallArg: true,
		}
		r.varList = append(r.varList, v)
		r.varMap[arg] = v
	}
	return r
}

func (r *routineInfo) getVar(varName string) (ASTVar, bool) {
	if !isIdent(varName) {
		return ASTVar{}, false
	}
	if v, ok := r.varMap[varName]; ok {
		return v, true
	}
	v := ASTVar{
		Name:      varName,
		Index:     len(r.varList),
		IsCallArg: false,
	}
	r.varList = append(r.varList, v)
	r.varMap[varName] = v
	return v, true
}

func (r *routineInfo) getVal(valName string) (*ASTVar, bool) {
	if valName == "0" {
		return nil, true
	}
	v, ok := r.getVar(valName)
	return &v, ok
}

func isIdent(s string) bool {
	if s == "" || s == "0" {
		return false
	}
	for _, ch := range s {
		if !unicode.IsLetter(ch) && !unicode.IsNumber(ch) {
			return false
		}
	}
	return true
}

func isVal(s string) bool {
	if s == "0" {
		return true
	}
	return isIdent(s)
}

func parseArgs(tokens []string) ([]string, bool) {
	if len(tokens) < 2 || tokens[0] != "(" || tokens[len(tokens)-1] != ")" {
		return nil, false
	}
	if len(tokens) >= 3 && len(tokens)%2 != 1 {
		return nil, false
	}
	var args []string
	for i, token := range tokens[1 : len(tokens)-1] {
		if i%2 == 0 && isVal(token) {
			args = append(args, token)
		} else if i%2 == 0 || token != "," {
			return nil, false
		}
	}
	return args, true
}

func hasReturn(statements []ASTStatement) (int, bool) {
	for _, statement := range statements {
		if statement.Type == StmtReturn {
			return statement.LineNumber, true
		}
		if lineNumber, hasRet := hasReturn(statement.Statements); hasRet {
			return lineNumber, true
		}
	}
	return 0, false
}

func checkCalls(astModule *ASTModule, statements []ASTStatement) error {
	for _, statement := range statements {
		if statement.Type == StmtCall && statement.CallTargetModule == "" {
			if _, ok := astModule.Subroutine[statement.CallTargetRoutine]; !ok {
				return errors.New(fmt.Sprintf("%s:%d: UNDEFINED SUBROUTINE", astModule.Filename, statement.LineNumber))
			}
		}
		if err := checkCalls(astModule, statement.Statements); err != nil {
			return err
		}
	}
	return nil
}
