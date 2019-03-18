package main

import (
	"bufio"
	"fmt"
	"io"
	"unicode"
)

type Token struct {
	Value        string
	Identifier   bool
	Filename     string
	Line, Column int
}

func (t Token) IsToken(tok string) bool {
	return !t.Identifier && tok == t.Value
}

type Tokenizer struct {
	filename string
	r        io.RuneReader

	buffer       []rune
	line, column int
	atEOF        bool
}

func NewTokenizer(filename string, r io.Reader) *Tokenizer {
	return &Tokenizer{
		filename: filename,
		r:        bufio.NewReader(r),
		line:     1,
		column:   1,
	}
}

func (t *Tokenizer) Next() (Token, error) {
	for {
		if len(t.buffer) == 0 {
			if err := t.readRune(); err != nil {
				return Token{}, err
			}
			if t.atEOF {
				return Token{}, io.EOF
			}
		}
		switch t.buffer[0] {
		case '\n':
			t.eatBuffer(1)
			t.line++
			t.column = 1
		case ' ', '\t', '\f', '\v', '\r':
			t.eatBuffer(1)
			t.column++
		case '+', '-', '=', '<', '>', '(', ')', '{', '}', '.', ',':
			tok := string(t.buffer[0])
			line := t.line
			column := t.column
			t.eatBuffer(1)
			t.column++
			return Token{
				Value:      tok,
				Identifier: false,
				Filename:   t.filename,
				Line:       line,
				Column:     column,
			}, nil
		case ':':
			if len(t.buffer) == 1 {
				if err := t.readRune(); err != nil {
					return Token{}, err
				}
			}
			if t.buffer[1] != '=' {
				return Token{}, fmt.Errorf("%s:%d:%d: Unrecognized token", t.filename, t.line, t.column)
			}
			line := t.line
			column := t.column
			t.eatBuffer(2)
			t.column += 2
			return Token{
				Value:      ":=",
				Identifier: false,
				Filename:   t.filename,
				Line:       line,
				Column:     column,
			}, nil
		case '/':
			if len(t.buffer) == 1 {
				if err := t.readRune(); err != nil {
					return Token{}, err
				}
			}
			if t.buffer[1] != '/' {
				return Token{}, fmt.Errorf("%s:%d:%d: Unrecognized token", t.filename, t.line, t.column)
			}
			for i := 2; i < len(t.buffer); i++ {
				if t.buffer[i] == '\n' {
					t.eatBuffer(i)
				}
			}
			for t.buffer[0] != '\n' {
				t.buffer = t.buffer[:0]
				if err := t.readRune(); err != nil {
					return Token{}, err
				}
				if t.atEOF {
					return Token{}, io.EOF
				}
			}
		case '"':
			escaped := false
			for i := 1; i < len(t.buffer); i++ {
				if escaped {
					escaped = false
				} else if t.buffer[i] == '\\' {
					escaped = true
				} else if t.buffer[i] == '"' {
					return t.quotedIdentifier(i), nil
				}
			}
			for {
				if err := t.readRune(); err != nil {
					return Token{}, err
				}
				if t.atEOF {
					return Token{}, fmt.Errorf("%s:%d:%d: Unterminated identifier", t.filename, t.line, t.column)
				}
				if escaped {
					escaped = false
				} else if t.buffer[len(t.buffer)-1] == '\\' {
					escaped = true
				} else if t.buffer[len(t.buffer)-1] == '"' {
					return t.quotedIdentifier(len(t.buffer) - 1), nil
				}
			}
		default:
			if !isIdentifierChar(t.buffer[0]) {
				return Token{}, fmt.Errorf("%s:%d:%d: Unrecognized token", t.filename, t.line, t.column)
			}
			for i := 1; i < len(t.buffer); i++ {
				if !isIdentifierChar(t.buffer[i]) {
					return t.identifier(i), nil
				}
			}
			for {
				if err := t.readRune(); err != nil {
					return Token{}, err
				}
				if t.atEOF {
					return t.identifier(len(t.buffer)), nil
				}
				if !isIdentifierChar(t.buffer[len(t.buffer)-1]) {
					return t.identifier(len(t.buffer) - 1), nil
				}
			}
		}
	}
}

func (t *Tokenizer) eatBuffer(n int) {
	copy(t.buffer, t.buffer[n:])
	t.buffer = t.buffer[:len(t.buffer)-n]
}

func (t *Tokenizer) readRune() error {
	if t.atEOF {
		return nil
	}
	r, _, err := t.r.ReadRune()
	if err == io.EOF {
		t.atEOF = true
		return nil
	} else if err != nil {
		return err
	}
	t.buffer = append(t.buffer, r)
	return nil
}

func (t *Tokenizer) identifier(i int) Token {
	value := string(t.buffer[:i])
	line := t.line
	column := t.column
	t.eatBuffer(i)
	t.column += i
	return Token{
		Value:      value,
		Identifier: value != "return" && value != "0",
		Filename:   t.filename,
		Line:       line,
		Column:     column,
	}
}

func (t *Tokenizer) quotedIdentifier(i int) Token {
	line := t.line
	column := t.column
	for j := 1; j <= i; j++ {
		if t.buffer[j] == '\n' {
			t.line++
			t.column = 1
		} else {
			t.column++
		}
	}
	n := i
	for j := 1; j < n; j++ {
		if t.buffer[j] == '\\' {
			copy(t.buffer[j:], t.buffer[j+1:n])
			n--
		}
	}
	value := string(t.buffer[1:n])
	t.eatBuffer(i + 1)
	return Token{
		Value:      value,
		Identifier: true,
		Filename:   t.filename,
		Line:       line,
		Column:     column,
	}
}

func isIdentifierChar(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}
