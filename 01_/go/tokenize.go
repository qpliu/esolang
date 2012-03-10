package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
)

type Token struct {
	Filename     string
	Line, Column int
	Token        string
}

func (t Token) Location() string {
	return fmt.Sprintf("%s:%d:%d", t.Filename, t.Line, t.Column)
}

func (t Token) IsLiteral() bool {
	switch t.Token[0] {
	case '0', '1', '_':
		return true
	}
	return false
}

func (t Token) IsTerminatedLiteral() bool {
	switch t.Token[0] {
	case '0', '1', '_':
		return t.Token[len(t.Token)-1] == '_'
	}
	return false
}

func (t Token) IsName() bool {
	switch t.Token[0] {
	case '0', '1', '_', '=', '.':
		return false
	}
	return true
}

func Tokenize(filenames []string) <-chan Token {
	result := make(chan Token)
	go func() {
		for _, filename := range filenames {
			for token := range tokenizeFile(filename) {
				result <- token
			}
		}
		close(result)
	}()
	return (<-chan Token)(result)
}

func tokenizeFile(filename string) <-chan Token {
	tokens := make(chan Token)
	file, err := os.Open(filename)
	if err != nil {
		panic(err.Error())
	}
	go func() {
		defer file.Close()
		line := 1
		column := 0
		var buf bytes.Buffer
		reader := bufio.NewReader(file)
		pendingLiteralLine, pendingLiteralColumn := 0, 0
		pendingNameLine, pendingNameColumn := 0, 0
		pendingEqualsLine, pendingEqualsColumn := 0, 0
		inComment := false
		finishToken := func(endLiterals bool) {
			if pendingLiteralLine != 0 && (endLiterals || pendingEqualsLine != 0) {
				tokens <- Token{filename, pendingLiteralLine, pendingLiteralColumn, buf.String()}
				pendingLiteralLine = 0
				buf.Reset()
			}
			if pendingNameLine != 0 {
				tokens <- Token{filename, pendingNameLine, pendingNameColumn, buf.String()}
				pendingNameLine = 0
				buf.Reset()
			}
			if pendingEqualsLine != 0 {
				tokens <- Token{filename, pendingEqualsLine, pendingEqualsColumn, "="}
				pendingEqualsLine = 0
			}
		}
		for {
			rune, _, err := reader.ReadRune()
			switch err {
			case nil:
				break
			case io.EOF:
				finishToken(true)
				close(tokens)
				return
			default:
				panic(err.Error())
			}
			column++
			if rune == '\n' {
				line++
				column = 0
				if inComment {
					inComment = false
				}
				continue
			}
			if inComment {
				continue
			}
			switch rune {
			case ' ', '\t', '\r':
				finishToken(false)
			case '=':
				if pendingEqualsLine == 0 {
					finishToken(true)
					pendingEqualsLine = line
					pendingEqualsColumn = column
				} else {
					pendingEqualsLine = 0
					inComment = true
				}
			case '0', '1':
				finishToken(false)
				if pendingLiteralLine == 0 {
					pendingLiteralLine = line
					pendingLiteralColumn = column
				}
				buf.WriteRune(rune)
			case '_':
				finishToken(false)
				buf.WriteRune(rune)
				if pendingLiteralLine == 0 {
					pendingLiteralLine = line
					pendingLiteralColumn = column
				}
				finishToken(true)
			case '.':
				finishToken(true)
				tokens <- Token{filename, line, column, "."}
			default:
				if pendingNameLine == 0 {
					finishToken(true)
					pendingNameLine = line
					pendingNameColumn = column
				}
				buf.WriteRune(rune)
			}
		}
	}()
	return (<-chan Token)(tokens)
}
