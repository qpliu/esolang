package tokenize

import (
	"testing"
	"time"
)

func testTokenize(s string, tokens []string, t *testing.T) {
	in := make(chan byte)
	out := make(chan string)
	go Tokenize(in, out)
	timeout := time.After(5 * time.Second)
	go func() {
		for _, b := range []byte(s) {
			in <- b
		}
	}()
	for _, token := range tokens {
		select {
		case <-timeout:
			t.Errorf("timeout: %s", s)
			return
		case tok, ok := <-out:
			if !ok {
				t.Errorf("unexpected eof: %s", s)
				return
			} else if tok != token {
				t.Errorf("unexpected token: expected %s, got %s: %s", token, tok, s)
				return
			}
		}
	}
}

func TestTokenize(t *testing.T) {
	testTokenize("a.=1a. == comment\nb=a0_.", []string{
		"a", ".", "=", "1", "a", ".", "b", "=", "a", "0_", ".",
	}, t)
	testTokenize("abc0 0 ==comment\n1 _ 001 = 1 abc.", []string{
		"abc","001_","001","=","1","abc",".",
	}, t)
}
