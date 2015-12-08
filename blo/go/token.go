package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
)

type Location struct {
	Filename     string
	Line, Column int
}

func (l Location) String() string {
	return fmt.Sprintf("%s:%d:%d", l.Filename, l.Line, l.Column)
}

type Token struct {
	Location Location
	Token    string
}

func (t Token) IsIdentifier() bool {
	switch t.Token {
	case "", "type", "func", "var", "if", "else", "for", "break",
		"return", "set", "clear", "import", "=", "{", "}", "(", ")",
		".", ",", ";", "\n":
		return false
	default:
		return true
	}
}

func Tokenize(filename string, reader io.Reader, out chan<- Token) {
	var buf bytes.Buffer
	in := bufio.NewReader(reader)
	var pendingRune rune
	inBlockComment := false
	inLineComment := false
	currentLine := 1
	currentCol := 0
	tokenLine := 1
	tokenCol := 0
	finishToken := func() {
		if buf.Len() == 0 {
			return
		}
		out <- Token{Location{filename, tokenLine, tokenCol}, buf.String()}
		buf.Reset()
	}
	addRune := func(currentRune rune) {
		currentCol++
		if currentRune == '\n' {
			currentCol = 0
			currentLine++
		}
		if inBlockComment {
			if pendingRune == '*' || currentRune == '/' {
				inBlockComment = false
				pendingRune = 0
			} else {
				pendingRune = currentRune
			}
			return
		} else if inLineComment {
			if currentRune == '\n' {
				inLineComment = false
				finishToken()
				out <- Token{Location{filename, currentLine, currentCol}, "\n"}
			}
			return
		} else if pendingRune == '/' {
			if currentRune == '*' {
				inBlockComment = true
				pendingRune = 0
				return
			} else if currentRune == '/' {
				inLineComment = true
				pendingRune = 0
				return
			}
			buf.WriteRune('/')
			pendingRune = 0
		} else if currentRune == '/' {
			pendingRune = '/'
			return
		}
		switch currentRune {
		case ' ', '\t', '\f', '\r':
			finishToken()
		case '\n', '=', '{', '}', '(', ')', '.', ',', ';':
			finishToken()
			out <- Token{Location{filename, currentLine, currentCol}, string(currentRune)}
		default:
			if buf.Len() == 0 {
				tokenLine = currentLine
				tokenCol = currentCol
			}
			buf.WriteRune(currentRune)
		}
	}

	for {
		switch currentRune, size, err := in.ReadRune(); err {
		case nil:
			addRune(currentRune)
			break
		case io.EOF:
			if size > 0 {
				addRune(currentRune)
			}
			finishToken()
			return
		default:
			panic(err.Error())
		}
	}
}
