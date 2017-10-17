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
		return program, err
	}
	for {
		if ok, tokens := tokenizeLine(line, "USE"); ok {
			if len(tokens) != 1 || !isIdentifier(tokens[0]) {
				return program, errors.New("SYNTAX ERROR: " + line)
			}
			program.uses = append(program.uses, tokens[0])
		} else if strings.HasPrefix(line, "SUBROUTINE") || strings.HasPrefix(line, "PROGRAM") {
			break
		} else {
			return program, errors.New("SYNTAX ERROR: " + line)
		}
		line, err = readLine(r)
		if err != nil {
			return program, err
		}
	}
	for {
		if ok, tokens := tokenizeLine(line, "SUBROUTINE"); ok {
			_ = tokens
			// TODO
		} else if ok, tokens := tokenizeLine(line, "PROGRAM"); ok {
			_ = tokens
			// TODO
			break
		} else {
			return program, errors.New("SYNTAX ERROR: " + line)
		}
	}
	line, err = readLine(r)
	if err != io.EOF {
		return program, errors.New("SYNTAX ERROR: " + line)
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
	if _, err := io.WriteString(w, "PROGRAM "+program.name+"\n"); err != nil {
		return err
	}
	for _, statement := range program.statements {
		const indent = 1
		if err := unparseStatement(w, indent, statement); err != nil {
			return err
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
			panic("INVALID IF STATEMENT")
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
					panic("INVALID IF STATEMENT")
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
					panic("INVALID IF STATEMENT")
				}
				if _, err := io.WriteString(w, "ELSE\n"); err != nil {
					return err
				}
			default:
				panic("INVALID IF STATEMENT")
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
		panic("INVALID STATEMENT")
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
		if _, err := io.WriteString(w, "CALL "+statement.parameters[0]); err != nil {
			return err
		}
		if err := unparseParameters(w, statement.parameters[1:]); err != nil {
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
		panic("INVALID STATEMENT")
	}
	return nil
}
