package main

import (
	"bytes"
	"fmt"
	"testing"
)

func TestTokenize(t *testing.T) {
	tok := NewTokenizer(bytes.NewBufferString(`
a<b.  == This is a comment
c {
  d=(e<f)g.
  h>i.
  j=testing.
  break 0 0.
  klmnop+{
    q = 0.
  }
}`), "")
	expect := func(expected string, line, col int, isIdent bool) {
		token, ok := tok.Token()
		if !ok {
			t.Errorf("expected token %s at %d:%d, got EOF", expected, line, col)
		} else if !ok || token.T != expected || line != token.Line || col != token.Column || token.Loc() != fmt.Sprintf(":%d:%d", line, col) {
			t.Errorf("expected token %s at %d:%d, got token %s at %d:%d", expected, line, col, token.T, token.Line, token.Column)
		}
		if isIdent != token.IsIdent() {
			t.Errorf("%s: expected isIdent=%v, got %v", token.Loc(), isIdent, token.IsIdent())
		}
	}

	expect("a", 2, 1, true)
	tok.Next()
	expect("<", 2, 2, false)
	tok.Next()
	tok.Push(Token{"z", "", 1, 1})
	expect("z", 1, 1, true)
	tok.Next()
	expect("b", 2, 3, true)
	tok.Next()
	expect(".", 2, 4, false)
	tok.Next()
	expect("c", 3, 1, true)
	tok.Next()
	expect("{", 3, 3, false)
	tok.Next()
	expect("d", 4, 3, true)
	tok.Next()
	expect("=", 4, 4, false)
	tok.Next()
	expect("(", 4, 5, false)
	tok.Next()
	expect("e", 4, 6, true)
	tok.Next()
	expect("<", 4, 7, false)
	tok.Next()
	expect("f", 4, 8, true)
	tok.Next()
	expect(")", 4, 9, false)
	tok.Next()
	expect("g", 4, 10, true)
	tok.Next()
	expect(".", 4, 11, false)
	tok.Next()
	expect("h", 5, 3, true)
	tok.Next()
	expect(">", 5, 4, false)
	tok.Next()
	expect("i", 5, 5, true)
	tok.Next()
	expect(".", 5, 6, false)
	tok.Next()
	expect("j", 6, 3, true)
	tok.Next()
	expect("=", 6, 4, false)
	tok.Next()
	expect("testing", 6, 5, true)
	tok.Next()
	expect(".", 6, 12, false)
	tok.Next()
	expect("break", 7, 3, false)
	tok.Next()
	expect("0", 7, 9, true)
	tok.Next()
	expect("0", 7, 11, true)
	tok.Next()
	expect(".", 7, 12, false)
	tok.Next()
	expect("klmnop", 8, 3, true)
	tok.Next()
	expect("+", 8, 9, false)
	tok.Next()
	expect("{", 8, 10, false)
	tok.Next()
	expect("q", 9, 5, true)
	tok.Next()
	expect("=", 9, 7, false)
	tok.Next()
	expect("0", 9, 9, true)
	tok.Next()
	expect(".", 9, 10, false)
	tok.Next()
	expect("}", 10, 3, false)
	tok.Next()
	expect("}", 11, 1, false)
	tok.Next()
	token, ok := tok.Token()
	if ok {
		t.Errorf("expected EOF, got token %s at %d:%d", token.T, token.Line, token.Column)
	}

	tok = NewTokenizer(bytes.NewBufferString("abc=="), "")
	expect("abc", 1, 1, true)
	tok.Next()
	token, ok = tok.Token()
	if ok {
		t.Errorf("expected EOF, got token %s at %d:%d", token.T, token.Line, token.Column)
	}

	tok = NewTokenizer(bytes.NewBufferString("abcd ="), "")
	expect("abcd", 1, 1, true)
	tok.Next()
	expect("=", 1, 6, false)
	tok.Next()
	token, ok = tok.Token()
	if ok {
		t.Errorf("expected EOF, got token %s at %d:%d", token.T, token.Line, token.Column)
	}
	if tok.Loc() != ":1:6" {
		t.Errorf("expected :1:6, got %s", tok.Loc())
	}
	token = Token{}
	if token.Loc() != "implicit" {
		t.Errorf("expected implicit, got %s", token.Loc())
	}
}
