package repl

import (
	"bytes"
	"io"
	"testing"
)

type testTokenizer struct {
	buffer bytes.Buffer
}

func (t *testTokenizer) Read(b byte) error {
	return t.buffer.WriteByte(b)
}

func (t *testTokenizer) Finalize() {
}

func (t *testTokenizer) Tokens() (result []string) {
	result = []string{t.buffer.String()}
	t.buffer.Reset()
	return
}

func testReader(s string, expected []string, t *testing.T) {
	var tokenizer testTokenizer
	r := newReader(&tokenizer, bytes.NewReader([]byte(s)))
	var results []string
	for {
		err := r.read()
		if err != nil && err != io.EOF {
			t.Errorf("read error: err=%s, s=%s", err.Error(), s)
			return
		}
		tokens := tokenizer.Tokens()
		if len(tokens) != 1 {
			t.Errorf("len(tokens)=%d != 1, s=%s", len(tokens), s)
			return
		}
		results = append(results, tokens[0])
		if err == io.EOF {
			break
		}
	}
	if len(results) != len(expected) {
		t.Errorf("wrong number of lines=%d, expected=%d, s=%s", len(results), len(expected), s)
		return
	}
	for i := range results {
		if results[i] != expected[i] {
			t.Errorf("wrong line=%s, expected=%s, s=%s", results[i], expected[i], s)
			return
		}
	}
}

func TestReader(t *testing.T) {
	testReader("first line\nsecond line\nthird line", []string{
		"first line\n", "second line\n", "third line",
	}, t)
}
