package main

import (
	"bytes"
	"fmt"
	"testing"
)

func TestTokenize(t *testing.T) {
	tok := NewTokenizer(bytes.NewBufferString(`
main system { == comment
  [ a = null system < self {
      [ a _ < system {
          break
        }
      ]
    }
  ]
  [ in a < system {
      break
    }
  ]
  [ system < null {
      break
    }
  ]
}`), "")
	expect := func(expected string, line, col int, isIdent, isExpr bool) {
		token, ok := tok.Token()
		if !ok {
			t.Errorf("expected token %s at %d:%d, got EOF", expected, line, col)
		} else if !ok || token.T != expected || line != token.Line || col != token.Column || token.Loc() != fmt.Sprintf(":%d:%d", line, col) {
			t.Errorf("expected token %s at %d:%d, got token %s at %d:%d", expected, line, col, token.T, token.Line, token.Column)
		}
		if isIdent != token.IsIdent() {
			t.Errorf("%s: expected isIdent=%v, got %v", token.Loc(), isIdent, token.IsIdent())
		}
		if isExpr != token.IsExpr() {
			t.Errorf("%s: expected isExpr=%v, got %v", token.Loc(), isIdent, token.IsIdent())
		}
	}

	expect("main", 2, 1, true, true)
	tok.Next()
	expect("system", 2, 6, true, true)
	tok.Next()
	expect("{", 2, 13, false, false)
	tok.Next()
	tok.Push(Token{"z", "", 1, 1})
	expect("z", 1, 1, true, true)
	tok.Next()
	expect("[", 3, 3, false, false)
	tok.Next()
	expect("a", 3, 5, true, true)
	tok.Next()
	expect("=", 3, 7, false, false)
	tok.Next()
	expect("null", 3, 9, false, true)
	tok.Next()
	expect("system", 3, 14, true, true)
	tok.Next()
	expect("<", 3, 21, false, false)
	tok.Next()
	expect("self", 3, 23, false, true)
	tok.Next()
	expect("{", 3, 28, false, false)
	tok.Next()
	expect("[", 4, 7, false, false)
	tok.Next()
	expect("a", 4, 9, true, true)
	tok.Next()
	expect("_", 4, 11, true, true)
	tok.Next()
	expect("<", 4, 13, false, false)
	tok.Next()
	expect("system", 4, 15, true, true)
	tok.Next()
	expect("{", 4, 22, false, false)
	tok.Next()
	expect("break", 5, 11, false, false)
	tok.Next()
	expect("}", 6, 9, false, false)
	tok.Next()
	expect("]", 7, 7, false, false)
	tok.Next()
	expect("}", 8, 5, false, false)
	tok.Next()
	expect("]", 9, 3, false, false)
	tok.Next()
	expect("[", 10, 3, false, false)
	tok.Next()
	expect("in", 10, 5, true, true)
	tok.Next()
	expect("a", 10, 8, true, true)
	tok.Next()
	expect("<", 10, 10, false, false)
	tok.Next()
	expect("system", 10, 12, true, true)
	tok.Next()
	expect("{", 10, 19, false, false)
	tok.Next()
	expect("break", 11, 7, false, false)
	tok.Next()
	expect("}", 12, 5, false, false)
	tok.Next()
	expect("]", 13, 3, false, false)
	tok.Next()
	expect("[", 14, 3, false, false)
	tok.Next()
	expect("system", 14, 5, true, true)
	tok.Next()
	expect("<", 14, 12, false, false)
	tok.Next()
	expect("null", 14, 14, false, true)
	tok.Next()
	expect("{", 14, 19, false, false)
	tok.Next()
	expect("break", 15, 7, false, false)
	tok.Next()
	expect("}", 16, 5, false, false)
	tok.Next()
	expect("]", 17, 3, false, false)
	tok.Next()
	expect("}", 18, 1, false, false)
	tok.Next()
	token, ok := tok.Token()
	if ok {
		t.Errorf("expected EOF, got token %s at %d:%d", token.T, token.Line, token.Column)
	}

	tok = NewTokenizer(bytes.NewBufferString("abc=="), "")
	expect("abc", 1, 1, true, true)
	tok.Next()
	token, ok = tok.Token()
	if ok {
		t.Errorf("expected EOF, got token %s at %d:%d", token.T, token.Line, token.Column)
	}

	tok = NewTokenizer(bytes.NewBufferString("abcd ="), "")
	expect("abcd", 1, 1, true, true)
	tok.Next()
	expect("=", 1, 6, false, false)
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

	tok = NewTokenizer(bytes.NewBufferString("abc=["), "")
	expect("abc", 1, 1, true, true)
	tok.Next()
	expect("=", 1, 4, false, false)
	tok.Next()
	expect("[", 1, 5, false, false)
	tok.Next()
	token, ok = tok.Token()
	if ok {
		t.Errorf("expected EOF, got token %s at %d:%d", token.T, token.Line, token.Column)
	}

	tok = NewTokenizer(bytes.NewBufferString("abc=a"), "")
	expect("abc", 1, 1, true, true)
	tok.Next()
	expect("=", 1, 4, false, false)
	tok.Next()
	expect("a", 1, 5, true, true)
	tok.Next()
	token, ok = tok.Token()
	if ok {
		t.Errorf("expected EOF, got token %s at %d:%d", token.T, token.Line, token.Column)
	}
}
