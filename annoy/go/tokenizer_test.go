package main

import (
	"bytes"
	"io"
	"testing"
)

func testTokenize(filename, src string) ([]Token, error) {
	tokens := []Token{}
	tokenizer := NewTokenizer(filename, bytes.NewBufferString(src))
	for {
		token, err := tokenizer.Next()
		if err != nil {
			if err == io.EOF {
				err = nil
			}
			return tokens, err
		}
		tokens = append(tokens, token)
	}
}

func testTokenizer(t *testing.T, filename, src string, expected []string) []Token {
	tokens, err := testTokenize(filename, src)
	if err != nil {
		t.Errorf("%s: err=%s", filename, err.Error())
	}
	if len(tokens) != len(expected) {
		t.Errorf("%s: len(tokens)=%d != expected=%d", filename, len(tokens), len(expected))
	}
	for i, token := range tokens {
		if token.Value != expected[i] {
			t.Errorf("%s:%d:%d: token=%s != expected=%s", filename, token.Line, token.Column, token.Value, expected[i])
		}
	}
	return tokens
}

func testTokenizeError(t *testing.T, filename, src, errMsg string) []Token {
	tokens, err := testTokenize(filename, src)
	if err == nil {
		t.Errorf("%s: expected error=%s", filename, errMsg)
	} else if err.Error() != errMsg {
		t.Errorf("%s: error=%s != expected=%s", filename, err.Error(), errMsg)
	}
	return tokens
}

func checkTokens(t *testing.T, filename string, tokens, expected []Token) {
	if len(tokens) != len(expected) {
		t.Errorf("%s: len(tokens)=%d != expected=%d", filename, len(tokens), len(expected))
	}
	for i, token := range tokens {
		if token.Filename != expected[i].Filename {
			t.Errorf("%s:%d:%d: filename=%s != expected=%s", filename, token.Line, token.Column, token.Filename, expected[i].Filename)
		}
		if token.Line != expected[i].Line {
			t.Errorf("%s:%d:%d: line=%d != expected=%d", filename, token.Line, token.Column, token.Line, expected[i].Line)
		}
		if token.Column != expected[i].Column {
			t.Errorf("%s:%d:%d: column=%d != expected=%d", filename, token.Line, token.Column, token.Column, expected[i].Column)
		}
		if token.Value != expected[i].Value {
			t.Errorf("%s:%d:%d: %s != expected=%s", filename, token.Line, token.Column, token.Value, expected[i].Value)
		}
		if token.Identifier != expected[i].Identifier {
			t.Errorf("%s:%d:%d: identifier=%v != expected=%v", filename, token.Line, token.Column, token.Identifier, expected[i].Identifier)
		}
	}
}

func TestTokenizer(t *testing.T) {
	testTokenizer(t, "test", "{a:=0+(0+0).return a}.", []string{"{", "a", ":=", "0", "+", "(", "0", "+", "0", ")", ".", "return", "a", "}", "."})
	testTokenizer(t, "test 2", "f(x,y):={x+y.return 0}.f(0,0+0).", []string{"f", "(", "x", ",", "y", ")", ":=", "{", "x", "+", "y", ".", "return", "0", "}", ".", "f", "(", "0", ",", "0", "+", "0", ")", "."})

	tokens := testTokenizer(t, "test whitespace", "{\n  return 0+x\n}.\n", []string{"{", "return", "0", "+", "x", "}", "."})
	checkTokens(t, "test whitespace", tokens, []Token{
		Token{
			Value:      "{",
			Identifier: false,
			Filename:   "test whitespace",
			Line:       1,
			Column:     1,
		},
		Token{
			Value:      "return",
			Identifier: false,
			Filename:   "test whitespace",
			Line:       2,
			Column:     3,
		},
		Token{
			Value:      "0",
			Identifier: false,
			Filename:   "test whitespace",
			Line:       2,
			Column:     10,
		},
		Token{
			Value:      "+",
			Identifier: false,
			Filename:   "test whitespace",
			Line:       2,
			Column:     11,
		},
		Token{
			Value:      "x",
			Identifier: true,
			Filename:   "test whitespace",
			Line:       2,
			Column:     12,
		},
		Token{
			Value:      "}",
			Identifier: false,
			Filename:   "test whitespace",
			Line:       3,
			Column:     1,
		},
		Token{
			Value:      ".",
			Identifier: false,
			Filename:   "test whitespace",
			Line:       3,
			Column:     2,
		},
	})

	testTokenizer(t, "test comment", "{\n  return //0+x\n}.\n", []string{"{", "return", "}", "."})
	testTokenizer(t, "test quoted identifier 1", "{\n  return 0+\"x y\"\n}.\n", []string{"{", "return", "0", "+", "x y", "}", "."})
	testTokenizer(t, "test quoted identifier 2", "{\n  return 0+\"x \\\" \\\\ \"\n}.\n", []string{"{", "return", "0", "+", "x \" \\ ", "}", "."})
	tokens = testTokenizer(t, "test quoted identifier 3", " \"x\ny\":=0.\n", []string{"x\ny", ":=", "0", "."})
	checkTokens(t, "test quoted identifier 3", tokens, []Token{
		Token{
			Value:      "x\ny",
			Identifier: true,
			Filename:   "test quoted identifier 3",
			Line:       1,
			Column:     2,
		},
		Token{
			Value:      ":=",
			Identifier: false,
			Filename:   "test quoted identifier 3",
			Line:       2,
			Column:     3,
		},
		Token{
			Value:      "0",
			Identifier: false,
			Filename:   "test quoted identifier 3",
			Line:       2,
			Column:     5,
		},
		Token{
			Value:      ".",
			Identifier: false,
			Filename:   "test quoted identifier 3",
			Line:       2,
			Column:     6,
		},
	})

	testTokenizeError(t, "unrecognized token", " /* error */", "unrecognized token:1:2: Unrecognized token")
	testTokenizeError(t, "unrecognized token 2", "  ** error **", "unrecognized token 2:1:3: Unrecognized token")
	testTokenizeError(t, "unrecognized token 3", "  :+ error +:", "unrecognized token 3:1:3: Unrecognized token")
	testTokenizeError(t, "unterminated identifier", "x := \"foo", "unterminated identifier:1:6: Unterminated identifier")
}
