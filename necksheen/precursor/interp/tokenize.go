package main

import (
	"bufio"
	"io"
	"strings"
	"unicode"
)

type Tokenizer struct {
	in     *bufio.Reader
	eof    bool
	buffer strings.Builder
	tokens []string
}

func NewTokenizer(in io.Reader) *Tokenizer {
	return &Tokenizer{
		in: bufio.NewReader(in),
	}
}

func (t *Tokenizer) Token() (string, bool) {
	if len(t.tokens) == 0 && !t.eof {
	loop:
		for {
			r, _, err := t.in.ReadRune()
			if err != nil {
				t.eof = true
				break loop
			}
			if unicode.IsSpace(r) {
				if t.buffer.Len() == 0 {
					continue
				}
				break loop
			}
			switch r {
			case '.', '<', '>', '(', ')', '{', '}':
				if t.buffer.Len() > 0 {
					t.tokens = append(t.tokens, t.buffer.String())
					t.buffer.Reset()
				}
				t.buffer.WriteRune(r)
				break loop
			case '=':
				if t.buffer.Len() > 0 {
					t.tokens = append(t.tokens, t.buffer.String())
					t.buffer.Reset()
				}
				r2, _, err2 := t.in.ReadRune()
				if err2 != nil {
					t.eof = true
					t.tokens = append(t.tokens, "=")
					break loop
				}
				if unicode.IsSpace(r2) {
					t.tokens = append(t.tokens, "=")
					break loop
				}
				switch r2 {
				case '=':
					for {
						r3, _, err3 := t.in.ReadRune()
						if err3 != nil {
							t.eof = true
							break loop
						} else if r3 == '\n' {
							break
						}
					}
					continue
				case '.', '<', '>', '(', ')', '{', '}':
					t.tokens = append(t.tokens, "=")
					t.buffer.WriteRune(r2)
				default:
					t.tokens = append(t.tokens, "=")
					t.buffer.WriteRune(r2)
					continue
				}
				break loop
			default:
				t.buffer.WriteRune(r)
				continue
			}
		}
		if t.buffer.Len() > 0 {
			t.tokens = append(t.tokens, t.buffer.String())
			t.buffer.Reset()
		}
	}
	if len(t.tokens) > 0 {
		return t.tokens[0], true
	} else {
		return "", false
	}
}

func (t *Tokenizer) Next() {
	if len(t.tokens) > 0 {
		t.tokens = t.tokens[1:]
	}
}

func (t *Tokenizer) Push(token string) {
	t.tokens = append(t.tokens, token)
	copy(t.tokens[1:], t.tokens[0:])
	t.tokens[0] = token
}
