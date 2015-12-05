package main

import (
	"strings"
	"testing"
)

func testTokenize1(t *testing.T, label, input string, expectedTokens []Token) {
	tokens := make(chan Token)
	go func() {
		Tokenize(strings.NewReader(input), tokens)
		close(tokens)
	}()
	i := 0
	for tok := range tokens {
		if i >= len(expectedTokens) {
			t.Errorf("%s: Expected %d tokens, got more", label, len(expectedTokens))
			return
		}
		if tok != expectedTokens[i] {
			t.Errorf("%s: Expected `%s', got `%s'", label, expectedTokens[i], tok)
			return
		}
		i++
	}
	if i != len(expectedTokens) {
		t.Errorf("%s: Expected %d tokens, got %d", label, len(expectedTokens), i)
		return
	}
}

func TestTokenize(t *testing.T) {
	testTokenize1(t, "blank main", "func main() {\n}\n", []Token{"func", "main", "(", ")", "{", "\n", "}", "\n"})
	testTokenize1(t, "comments", "func main() { // comment\n /* }\n*/", []Token{"func", "main", "(", ")", "{", "\n"})
	testTokenize1(t, "struct", "type i8 {\n    1, 2, 4, 8, 16, 32, 64, 128\n}", []Token{"type", "i8", "{", "\n", "1", ",", "2", ",", "4", ",", "8", ",", "16", ",", "32", ",", "64", ",", "128", "\n", "}"})
	testTokenize1(t, "assignment and field access", "func sum1(a, b i1) i1+carry {\n   var sum i1+carry\n    sum.i1 = a\n    if a.1 {\n        if b.1 {\n            set sum.carry; clear sum.i1.1\n        }\n    } else if b.1 {\n        set sum.i1.1\n    }\n    return sum\n}", []Token{"func", "sum1", "(", "a", ",", "b", "i1", ")", "i1+carry", "{", "\n", "var", "sum", "i1+carry", "\n", "sum", ".", "i1", "=", "a", "\n", "if", "a", ".", "1", "{", "\n", "if", "b", ".", "1", "{", "\n", "set", "sum", ".", "carry", ";", "clear", "sum", ".", "i1", ".", "1", "\n", "}", "\n", "}", "else", "if", "b", ".", "1", "{", "\n", "set", "sum", ".", "i1", ".", "1", "\n", "}", "\n", "return", "sum", "\n", "}"})
}
