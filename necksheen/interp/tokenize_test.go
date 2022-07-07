package main

import (
	"bytes"
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
}`))
	expect := func(expected string) {
		token, ok := tok.Token()
		if !ok {
			t.Errorf("expected token %s, got EOF", expected)
		} else if !ok || token != expected {
			t.Errorf("expected token %s, got token %s", expected, token)
		}
	}

	expect("a")
	tok.Next()
	expect("<")
	tok.Next()
	tok.Push("z")
	expect("z")
	tok.Next()
	expect("b")
	tok.Next()
	expect(".")
	tok.Next()
	expect("c")
	tok.Next()
	expect("{")
	tok.Next()
	expect("d")
	tok.Next()
	expect("=")
	tok.Next()
	expect("(")
	tok.Next()
	expect("e")
	tok.Next()
	expect("<")
	tok.Next()
	expect("f")
	tok.Next()
	expect(")")
	tok.Next()
	expect("g")
	tok.Next()
	expect(".")
	tok.Next()
	expect("h")
	tok.Next()
	expect(">")
	tok.Next()
	expect("i")
	tok.Next()
	expect(".")
	tok.Next()
	expect("j")
	tok.Next()
	expect("=")
	tok.Next()
	expect("testing")
	tok.Next()
	expect(".")
	tok.Next()
	expect("break")
	tok.Next()
	expect("0")
	tok.Next()
	expect("0")
	tok.Next()
	expect(".")
	tok.Next()
	expect("}")
	tok.Next()
	token, ok := tok.Token()
	if ok {
		t.Errorf("expected EOF, got token %s", token)
	}
}
