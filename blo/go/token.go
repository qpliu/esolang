package main

import (
	"bufio"
	"bytes"
	"io"
)

type Token string

func (t Token) IsIdentifier() bool {
	switch t {
	case "", "type", "func", "var", "if", "else", "for", "break",
		"return", "set", "clear", "import", "=", "{", "}", "(", ")",
		".", ",", ";", "\n":
		return false
	default:
		return true
	}
}

func Tokenize(reader io.Reader, out chan<- Token) {
	var buf bytes.Buffer
	in := bufio.NewReader(reader)
	var pendingRune rune
	inBlockComment := false
	inLineComment := false
	finishToken := func() {
		if buf.Len() == 0 {
			return
		}
		out <- Token(buf.String())
		buf.Reset()
	}
	addRune := func(currentRune rune) {
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
				out <- Token('\n')
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
			out <- Token(currentRune)
		default:
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
