package main

import (
	"bufio"
	"fmt"
	"io"
	"strings"
	"unicode"
)

type Token struct {
	T string

	Filename     string
	Line, Column int
}

type Tokenizer struct {
	in        *bufio.Reader
	filename  string
	eof       bool
	line, col int

	buffer                strings.Builder
	bufferLine, bufferCol int

	tokens []Token
}

func (t Token) Loc() string {
	if t.Filename == "" && t.Line == 0 && t.Column == 0 {
		return "implicit"
	}
	return fmt.Sprintf("%s:%d:%d", t.Filename, t.Line, t.Column)
}

func (t Token) IsIdent() bool {
	if !t.IsExpr() {
		return false
	}
	switch t.T {
	case "null", "self":
		return false
	default:
		return true
	}
}

func (t Token) IsExpr() bool {
	switch t.T {
	case "break", "continue", "=", "!", "<", "[", "]", "{", "}":
		return false
	default:
		return true
	}
}

func NewTokenizer(in io.Reader, filename string) *Tokenizer {
	return &Tokenizer{
		in:       bufio.NewReader(in),
		filename: filename,
		line:     1,
		col:      0,
	}
}

func (t *Tokenizer) Loc() string {
	return fmt.Sprintf("%s:%d:%d", t.filename, t.line, t.col)
}

func (t *Tokenizer) readRune() (rune, error) {
	r, _, err := t.in.ReadRune()
	if err != nil {
		t.eof = true
		return r, err
	}
	if r == '\n' {
		t.line++
		t.col = 0
	} else {
		t.col++
	}
	if t.buffer.Len() == 0 {
		t.bufferLine = t.line
		t.bufferCol = t.col
	}
	return r, nil
}

func (t *Tokenizer) makeToken() bool {
	if t.buffer.Len() == 0 {
		return false
	}
	t.tokens = append(t.tokens, Token{
		T:        t.buffer.String(),
		Filename: t.filename,
		Line:     t.bufferLine,
		Column:   t.bufferCol,
	})
	t.buffer.Reset()
	t.bufferLine = t.line
	t.bufferCol = t.col
	return true
}

func (t *Tokenizer) Token() (Token, bool) {
	if len(t.tokens) == 0 && !t.eof {
	loop:
		for {
			r, err := t.readRune()
			if err != nil {
				break loop
			}
			if unicode.IsSpace(r) {
				if !t.makeToken() {
					continue loop
				} else {
					break loop
				}
			}
			switch r {
			case '!', '<', '[', ']', '{', '}':
				t.makeToken()
				t.buffer.WriteRune(r)
				break loop
			case '=':
				t.makeToken()
				t.buffer.WriteRune(r)
				r2, err := t.readRune()
				if err != nil {
					break loop
				}
				if unicode.IsSpace(r2) {
					break loop
				}
				switch r2 {
				case '=':
					t.buffer.Reset()
					for {
						r3, err := t.readRune()
						if err != nil {
							break loop
						} else if r3 == '\n' {
							break
						}
					}
				case '!', '<', '[', ']', '{', '}':
					t.makeToken()
					t.buffer.WriteRune(r2)
					break loop
				default:
					t.makeToken()
					t.buffer.WriteRune(r2)
				}
			default:
				t.buffer.WriteRune(r)
			}
		}
		t.makeToken()
	}
	if len(t.tokens) > 0 {
		return t.tokens[0], true
	} else {
		return Token{}, false
	}
}

func (t *Tokenizer) Next() {
	l := len(t.tokens)
	if l > 0 {
		copy(t.tokens, t.tokens[1:])
		t.tokens = t.tokens[:l-1]
	}
}

func (t *Tokenizer) Push(token Token) {
	t.tokens = append(t.tokens, token)
	copy(t.tokens[1:], t.tokens[0:])
	t.tokens[0] = token
}
