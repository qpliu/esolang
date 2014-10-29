package tokenize

import "testing"

func testTokenize(s string, expected []string, t *testing.T) {
	var tokenizer Tokenizer
	for _, b := range []byte(s) {
		if err := tokenizer.Read(b); err != nil {
			t.Errorf("read error: err=%s, s=%s", err.Error(), s)
			return
		}
	}
	tokenizer.Finalize()
	tokens := tokenizer.Tokens()
	if len(tokens) != len(expected) {
		t.Errorf("wrong number of tokens=%d, expected=%d, s=%s", len(tokens), len(expected), s)
		return
	}
	for i := range tokens {
		if tokens[i] != expected[i] {
			t.Errorf("wrong token=%s, expected=%s, s=%s", tokens[i], expected[i], s)
			return
		}
	}
}

func TestTokenize(t *testing.T) {
	testTokenize("a.=1a. == comment\nb=a0_.", []string{
		"a", ".", "=", "1", "a", ".", "b", "=", "a", "0_", ".",
	}, t)
	testTokenize("abc0 0 ==comment\n1 _ 001 = 1 abc.", []string{
		"abc", "001_", "001", "=", "1", "abc", ".",
	}, t)
}
