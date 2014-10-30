package repl

import (
	"bytes"
	"fmt"
	"testing"
)

type testState int

func (s *testState) Prompt() string {
	*s++
	return fmt.Sprintf("%d>", *s)
}

func (s *testState) Eval(tokens []string) string {
	return fmt.Sprintf("value=%s", tokens[0])
}

func testRepl(input, expected string, t *testing.T) {
	var tokenizer testTokenizer
	var out bytes.Buffer
	in := bytes.NewReader([]byte(input))
	state := testState(0)
	if err := Repl(in, &out, &state, &tokenizer); err != nil {
		t.Errorf("error: err=%s, input=%s", err.Error(), input)
		return
	}
	if expected != out.String() {
		t.Errorf("out=%s, expected=%s", out.String(), expected)
		return
	}
}

func TestRepl(t *testing.T) {
	testRepl("one\ntwo\n\nfour\n", "1>value=one\n\n2>value=two\n\n3>value=\n\n4>value=four\n\n5>", t)
}
