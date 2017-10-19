package main

import (
	"bufio"
	"bytes"
	"errors"
	"io"
	"strings"
	"unicode"
)

func parse(r *bufio.Reader) (*program, error) {
	program := &program{}
	line, err := readLine(r)
	if err != nil {
		return nil, err
	}
	for {
		if ok, tokens := tokenizeLine(line, "USE"); ok {
			if len(tokens) != 1 || !isIdentifier(tokens[0]) {
				return nil, errors.New("SYNTAX ERROR: " + line)
			}
			for _, use := range program.uses {
				if tokens[0] == use {
					return nil, errors.New("DUPLICATE USE: " + line)
				}
			}
			program.uses = append(program.uses, tokens[0])
		} else if strings.HasPrefix(line, "SUBROUTINE") || strings.HasPrefix(line, "PROGRAM") || strings.HasPrefix(line, "LIBRARY") {
			break
		} else {
			return nil, errors.New("SYNTAX ERROR: " + line)
		}
		line, err = readLine(r)
		if err != nil {
			return nil, err
		}
	}
	for {
		if ok, tokens := tokenizeLine(line, "SUBROUTINE"); ok {
			if len(tokens) < 3 || !isIdentifier(tokens[0]) || tokens[1] != "(" || tokens[len(tokens)-1] != ")" {
				return nil, errors.New("SYNTAX ERROR: " + line)
			}
			name := tokens[0]
			for _, subroutine := range program.subroutines {
				if name == subroutine.name {
					return nil, errors.New("DUPLICATE SUBROUTINE: " + line)
				}
			}
			const isSubroutine = true
			const isIf = false
			var doLoopIdents []string
			parameters, statements, _, err := readStatements(r, line, tokens, name, isSubroutine, isIf, doLoopIdents)
			if err != nil {
				return nil, err
			}
			program.subroutines = append(program.subroutines, subroutine{name, parameters, statements})
		} else if ok, tokens := tokenizeLine(line, "PROGRAM"); ok {
			if len(tokens) != 1 || !isIdentifier(tokens[0]) {
				return nil, errors.New("SYNTAX ERROR: " + line)
			}
			name := tokens[0]
			const isSubroutine = false
			const isIf = false
			var doLoopIdents []string
			parameters, statements, _, err := readStatements(r, line, tokens, name, isSubroutine, isIf, doLoopIdents)
			if err != nil {
				return nil, err
			}
			if len(parameters) != 0 {
				return nil, errors.New("SYNTAX ERROR: " + line)
			}
			program.name = name
			program.statements = statements
			break
		} else if ok, tokens := tokenizeLine(line, "LIBRARY"); ok {
			if len(tokens) != 1 || !isIdentifier(tokens[0]) {
				return nil, errors.New("SYNTAX ERROR: " + line)
			}
			name := tokens[0]
			library, err := readLibrary(r, name, program)
			if err != nil {
				return nil, err
			}
			program.name = name
			program.library = library
			break
		} else {
			return nil, errors.New("SYNTAX ERROR: " + line)
		}
		line, err = readLine(r)
		if err != nil {
			return nil, err
		}
	}
	line, err = readLine(r)
	if err != io.EOF {
		return nil, errors.New("SYNTAX ERROR: " + line)
	}

	uses := make(map[string]bool)
	for _, use := range program.uses {
		uses[use] = true
	}
	subroutines := make(map[string]bool)
	for _, subroutine := range program.subroutines {
		subroutines[subroutine.name] = true
	}
	for _, subroutine := range program.subroutines {
		if err := checkCalls(uses, subroutines, subroutine.statements); err != nil {
			return nil, err
		}
	}
	if err := checkCalls(uses, subroutines, program.statements); err != nil {
		return nil, err
	}

	return program, nil
}

func readLine(r *bufio.Reader) (string, error) {
	buf := bytes.Buffer{}
	inComment := false
	for {
		ch, _, err := r.ReadRune()
		if err == io.EOF && buf.Len() > 0 {
			return buf.String(), nil
		} else if err != nil {
			return "", err
		}
		if ch == '\n' {
			if buf.Len() == 0 {
				inComment = false
				continue
			}
			return buf.String(), nil
		}
		if !inComment {
			if unicode.IsSpace(ch) {
			} else if ch == '*' {
				inComment = true
			} else {
				buf.WriteRune(ch)
			}
		}
	}
}

func readStatements(r *bufio.Reader, line string, tokens []string, name string, isSubroutine bool, isIf bool, doLoopIdents []string) ([]string, []statement, string, error) {
	var parameters []string
	if len(tokens) > 1 && tokens[1] == "(" {
		const callParameters = false
		params, err := readParameters(line, tokens[1:], callParameters)
		if err != nil {
			return nil, nil, "", err
		}
		parameters = params
	}

	var statements []statement
	for {
		line, err := readLine(r)
		if err != nil {
			return nil, nil, "", err
		}
		if ok, tokens := tokenizeLine(line, "LET"); ok {
			if len(tokens) != 3 || !isIdentifier(tokens[0]) || !isIdentifierOr0(tokens[2]) {
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			stmtType := stmtLetEq
			switch tokens[1] {
			case "=":
			case ">":
				stmtType = stmtLetMkEdge
			case "<":
				stmtType = stmtLetRmEdge
				if !isIdentifier(tokens[2]) {
					return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
				}
			default:
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			statements = append(statements, statement{
				stmtType:   stmtType,
				parameters: []string{tokens[0], tokens[2]},
			})
		} else if ok, tokens := tokenizeLine(line, "IF"); ok {
			if len(tokens) != 3 || !isIdentifier(tokens[0]) || (tokens[1] != "=" && tokens[1] != ">") || !isIdentifier(tokens[2]) {
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			var ifBranches []statement
			for {
				_, ifBranchStmts, nextLine, err := readStatements(r, line, nil, "IF", isSubroutine, true, doLoopIdents)
				if err != nil {
					return nil, nil, "", err
				}
				ifBranch := statement{statements: ifBranchStmts}
				seenElse := false
				if len(tokens) == 0 {
					ifBranch.stmtType = stmtElse
					seenElse = true
				} else if len(tokens) != 3 || !isIdentifier(tokens[0]) || (tokens[1] != "=" && tokens[1] != ">") || !isIdentifier(tokens[2]) {
					return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
				} else if tokens[1] == "=" {
					ifBranch.stmtType = stmtIfEq
					ifBranch.parameters = []string{tokens[0], tokens[2]}
				} else if tokens[1] == ">" {
					ifBranch.stmtType = stmtIfEdge
					ifBranch.parameters = []string{tokens[0], tokens[2]}
				}
				ifBranches = append(ifBranches, ifBranch)
				line = nextLine
				if ok, tokens := tokenizeLine(line, "ENDIF"); ok {
					if len(tokens) != 0 {
						return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
					}
					break
				}
				if seenElse {
					return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
				}
				ok, tokens = tokenizeLine(line, "ELSEIF")
				if !ok {
					ok, tokens = tokenizeLine(line, "ELSE")
					if !ok {
						return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
					}
				}
			}
			statements = append(statements, statement{
				stmtType:   stmtIf,
				statements: ifBranches,
			})
		} else if ok, tokens := tokenizeLine(line, "ELSEIF"); ok {
			if !isIf || len(tokens) != 3 || !isIdentifier(tokens[0]) || (tokens[1] != "=" && tokens[1] != ">") || !isIdentifier(tokens[2]) {
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			return parameters, statements, line, nil
		} else if ok, tokens := tokenizeLine(line, "ELSE"); ok {
			if !isIf || len(tokens) != 0 {
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			return parameters, statements, line, nil
		} else if ok, tokens := tokenizeLine(line, "DO"); ok {
			statement := statement{}
			if len(tokens) == 1 {
				if !isIdentifier(tokens[0]) {
					return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
				}
				statement.stmtType = stmtDoLoop
				statement.parameters = []string{tokens[0]}
			} else if len(tokens) == 3 && isIdentifier(tokens[0]) && tokens[1] == "<" && isIdentifier(tokens[2]) {
				statement.stmtType = stmtDoEdges
				statement.parameters = []string{tokens[0], tokens[2]}
			} else {
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			doLoopIdents = append(doLoopIdents, tokens[0])
			_, doStatements, _, err := readStatements(r, line, nil, "DO", isSubroutine, false, doLoopIdents)
			if err != nil {
				return nil, nil, "", err
			}
			doLoopIdents = doLoopIdents[:len(doLoopIdents)-1]
			statement.statements = doStatements
			statements = append(statements, statement)
		} else if ok, tokens := tokenizeLine(line, "CALL"); ok {
			var stmtParams []string
			if len(tokens) < 3 || !isIdentifier(tokens[0]) {
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			if tokens[1] == "." && isIdentifier(tokens[2]) {
				stmtParams = append(stmtParams, tokens[0], tokens[2])
				tokens = tokens[3:]
			} else {
				stmtParams = append(stmtParams, "", tokens[0])
				tokens = tokens[1:]
			}
			callParams, err := readParameters(line, tokens, true)
			if err != nil {
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			for _, param := range callParams {
				stmtParams = append(stmtParams, param)
			}
			statements = append(statements, statement{
				stmtType:   stmtCall,
				parameters: stmtParams,
			})
		} else if ok, tokens := tokenizeLine(line, "RETURN"); ok {
			if !isSubroutine || len(tokens) != 0 {
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			statements = append(statements, statement{stmtType: stmtReturn})
		} else if ok, tokens := tokenizeLine(line, "EXIT"); ok {
			if len(tokens) != 1 || !isIdentifier(tokens[0]) {
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			withinDoLoop := false
			for _, doLoopIdent := range doLoopIdents {
				if tokens[0] == doLoopIdent {
					withinDoLoop = true
					break
				}
			}
			if !withinDoLoop {
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			statements = append(statements, statement{
				stmtType:   stmtExit,
				parameters: []string{tokens[0]},
			})
		} else if ok, tokens := tokenizeLine(line, "END"); ok {
			if len(tokens) != 1 || tokens[0] != name {
				return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
			}
			return parameters, statements, line, nil
		} else {
			return nil, nil, "", errors.New("SYNTAX ERROR: " + line)
		}
	}
}

func readParameters(line string, tokens []string, callParameters bool) ([]string, error) {
	var parameters []string
	if len(tokens) < 2 || tokens[0] != "(" || tokens[len(tokens)-1] != ")" {
		return nil, errors.New("SYNTAX ERROR: " + line)
	}
	for i, token := range tokens[1 : len(tokens)-1] {
		if i%2 == 0 {
			if callParameters {
				if !isIdentifierOr0(token) {
					return nil, errors.New("SYNTAX ERROR: " + line)
				}
			} else {
				if !isIdentifier(token) {
					return nil, errors.New("SYNTAX ERROR: " + line)
				}
				for _, parameter := range parameters {
					if token == parameter {
						return nil, errors.New("SYNTAX ERROR: " + line)
					}
				}
			}
			parameters = append(parameters, token)
		} else {
			if token != "," {
				return nil, errors.New("SYNTAX ERROR: " + line)
			}
		}
	}
	return parameters, nil
}

func readLibrary(r *bufio.Reader, name string, program *program) ([]string, error) {
	var library []string
	for {
		line, err := readLine(r)
		if err == io.EOF {
			return library, errors.New("UNEXPECTED EOF")
		} else if err != nil {
			return library, err
		}
		if ok, tokens := tokenizeLine(line, "SUBROUTINE"); ok {
			if len(tokens) != 1 || !isIdentifier(tokens[0]) {
				return library, errors.New("SYNTAX ERROR: " + line)
			}
			defined := false
			for _, subroutine := range program.subroutines {
				if tokens[0] == subroutine.name {
					defined = true
					break
				}
			}
			if !defined {
				return library, errors.New("UNDEFINED SUBROUTINE: " + tokens[0])
			}
			library = append(library, tokens[0])
		} else if ok, tokens := tokenizeLine(line, "END"); ok {
			if len(tokens) != 1 || tokens[0] != name {
				return library, errors.New("SYNTAX ERROR: " + line)
			}
			return library, nil
		} else {
			return library, errors.New("SYNTAX ERROR: " + line)
		}
	}
}

func tokenizeLine(line string, keyword string) (bool, []string) {
	if !strings.HasPrefix(line, keyword) {
		return false, nil
	}
	buf := bytes.Buffer{}
	var tokens []string
	for _, ch := range line[len(keyword):] {
		if unicode.IsLetter(ch) || unicode.IsNumber(ch) {
			buf.WriteRune(ch)
		} else {
			if buf.Len() != 0 {
				tokens = append(tokens, buf.String())
				buf.Reset()
			}
			tokens = append(tokens, string(ch))
		}
	}
	if buf.Len() != 0 {
		tokens = append(tokens, buf.String())
	}
	return true, tokens
}

func isIdentifierOr0(s string) bool {
	if len(s) == 0 {
		return false
	}
	for _, ch := range s {
		if !unicode.IsLetter(ch) && !unicode.IsNumber(ch) {
			return false
		}
	}
	return true
}

func isIdentifier(s string) bool {
	return s != "0" && isIdentifierOr0(s)
}

func checkCalls(uses, subroutines map[string]bool, statements []statement) error {
	for _, statement := range statements {
		if err := checkCalls(uses, subroutines, statement.statements); err != nil {
			return err
		}
		if statement.stmtType == stmtCall {
			if statement.parameters[0] == "" {
				if !subroutines[statement.parameters[1]] {
					return errors.New("UNDEFINED SUBROUTINE: " + statement.parameters[1])
				}
			} else if !uses[statement.parameters[0]] {
				return errors.New("UNDECLARED LIBRARY: " + statement.parameters[0])
			}
		}
	}
	return nil
}

func unparse(w io.Writer, program *program) error {
	for _, use := range program.uses {
		if _, err := io.WriteString(w, "USE "+use+"\n"); err != nil {
			return err
		}
	}
	for _, subroutine := range program.subroutines {
		if _, err := io.WriteString(w, "SUBROUTINE "+subroutine.name); err != nil {
			return err
		}
		if err := unparseParameters(w, subroutine.parameters); err != nil {
			return err
		}
		for _, statement := range subroutine.statements {
			const indent = 1
			if err := unparseStatement(w, indent, statement); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, "END "+subroutine.name+"\n"); err != nil {
			return err
		}
	}
	if len(program.library) > 0 {
		if len(program.statements) > 0 {
			return errors.New("INVALID MODULE")
		}
		if _, err := io.WriteString(w, "LIBRARY "+program.name+"\n"); err != nil {
			return err
		}
		for _, library := range program.library {
			const indent = 1
			if err := unparseIndent(w, indent); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "SUBROUTINE "+library+"\n"); err != nil {
				return err
			}
		}
	} else {
		if _, err := io.WriteString(w, "PROGRAM "+program.name+"\n"); err != nil {
			return err
		}
		for _, statement := range program.statements {
			const indent = 1
			if err := unparseStatement(w, indent, statement); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(w, "END "+program.name+"\n"); err != nil {
		return err
	}
	return nil
}

func unparseParameters(w io.Writer, parameters []string) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	for i, parameter := range parameters {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, parameter); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ")\n"); err != nil {
		return err
	}
	return nil
}

func unparseIndent(w io.Writer, indent int) error {
	for i := 0; i < indent; i++ {
		if _, err := io.WriteString(w, "  "); err != nil {
			return err
		}
	}
	return nil
}

func unparseStatement(w io.Writer, indent int, statement statement) error {
	if err := unparseIndent(w, indent); err != nil {
		return err
	}
	switch statement.stmtType {
	case stmtLetEq:
		if _, err := io.WriteString(w, "LET "+statement.parameters[0]+" = "+statement.parameters[1]+"\n"); err != nil {
			return err
		}
	case stmtLetMkEdge:
		if _, err := io.WriteString(w, "LET "+statement.parameters[0]+" > "+statement.parameters[1]+"\n"); err != nil {
			return err
		}
	case stmtLetRmEdge:
		if _, err := io.WriteString(w, "LET "+statement.parameters[0]+" < "+statement.parameters[1]+"\n"); err != nil {
			return err
		}
	case stmtIf:
		if len(statement.statements) == 0 {
			return errors.New("INVALID IF STATEMENT")
		}
		seenElse := false
		for i, stmt := range statement.statements {
			if i > 0 {
				if err := unparseIndent(w, indent); err != nil {
					return err
				}
			}
			switch stmt.stmtType {
			case stmtIfEq, stmtIfEdge:
				if seenElse {
					return errors.New("INVALID IF STATEMENT")
				}
				if i > 0 {
					if _, err := io.WriteString(w, "ELSE "); err != nil {
						return err
					}
				}
				if stmt.stmtType == stmtIfEq {
					if _, err := io.WriteString(w, "IF "+stmt.parameters[0]+" = "+stmt.parameters[1]+"\n"); err != nil {
						return err
					}
				} else {
					if _, err := io.WriteString(w, "IF "+stmt.parameters[0]+" > "+stmt.parameters[1]+"\n"); err != nil {
						return err
					}
				}
			case stmtElse:
				if i == 0 {
					return errors.New("INVALID IF STATEMENT")
				}
				if _, err := io.WriteString(w, "ELSE\n"); err != nil {
					return err
				}
			default:
				return errors.New("INVALID IF STATEMENT")
			}
			for _, innerStmt := range stmt.statements {
				if err := unparseStatement(w, indent+1, innerStmt); err != nil {
					return err
				}
			}
		}
		if err := unparseIndent(w, indent); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "END IF\n"); err != nil {
			return err
		}
		return nil
	case stmtIfEq, stmtIfEdge, stmtElse:
		return errors.New("INVALID STATEMENT")
	case stmtDoLoop, stmtDoEdges:
		if statement.stmtType == stmtDoLoop {
			if _, err := io.WriteString(w, "DO "+statement.parameters[0]+"\n"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, "DO "+statement.parameters[0]+" < "+statement.parameters[1]+"\n"); err != nil {
				return err
			}
		}
		for _, stmt := range statement.statements {
			if err := unparseStatement(w, indent+1, stmt); err != nil {
				return err
			}
		}
		if err := unparseIndent(w, indent); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "END DO\n"); err != nil {
			return err
		}
	case stmtCall:
		if statement.parameters[0] == "" {
			if _, err := io.WriteString(w, "CALL "+statement.parameters[1]); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, "CALL "+statement.parameters[0]+"."+statement.parameters[1]); err != nil {
				return err
			}
		}
		if err := unparseParameters(w, statement.parameters[2:]); err != nil {
			return err
		}
	case stmtReturn:
		if _, err := io.WriteString(w, "RETURN\n"); err != nil {
			return err
		}
	case stmtExit:
		if _, err := io.WriteString(w, "EXIT "+statement.parameters[0]+"\n"); err != nil {
			return err
		}
	default:
		return errors.New("INVALID STATEMENT")
	}
	return nil
}
