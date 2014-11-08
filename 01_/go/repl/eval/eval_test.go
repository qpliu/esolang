package eval

import "testing"

func testEval(label string, lines [][]string, t *testing.T) {
	var state State
	for i, line := range lines {
		prompt := state.Prompt()
		if prompt != line[0] {
			t.Errorf("%s[%d] unexpected prompt: %s, expected: %s", label, i, prompt, line[0])
			return
		}
		result := state.Eval(line[2:])
		if result != line[1] {
			t.Errorf("%s[%d] unexpected result: %s, expected: %s", label, i, result, line[1])
		}
	}
}

func TestEval(t *testing.T) {
	testEval("testEval1", [][]string{
		{". ", ""},
		{". ", "", "0_", "0_"},
		{"... ", "000_", "0", "."},
		{". ", "", "id", "a", "=", "a", "."},
		{". ", "01_", "_", "id", "id", "id", "01_", "."},
		{". ", "01_", "_", "id", "0_", "id", "id", "1_", "."},
		{". ", "", ".", "-", "id"},
		{". ", "undefined symbol:id", "_", "id", "01_", "."},
		{". ", "", "id", "a", "=", "a", "."},
		{". ", "01_", "_", "id", "01_", "."},
		{". ", "", "concat", "a", "b", "=", "a", "b", "."},
		{". ", "01_", "_", "0_", "1_", "."},
		{". ", "01_", "_", "concat", "0_", "1_", "."},
		{". ", "01_", "_", "id", "concat", "0_", "1_", "."},
		{". ", "01_", "_", "concat", "id", "0_", "1_", "."},
		{". ", "", "snd", ".", "a", "=", "a", "."},
		{". ", "01_", "_", "snd", "11_", "id", "_", "01_", "."},
	}, t)
	testEval("testEval2", [][]string{
		{". ", "", "f", "a", "b", "c", "=", "a", "concat", "id", "b", "c", "."},
		{". ", "not enough arguments to a function", "=", "f", "01_", "."},
		{". ", "undefined symbol:concat", "=", "f", "0_", "1_", "0_", "."},
		{". ", "", "concat", "a", "b", "=", "a", "b", "."},
		{". ", "undefined symbol:id", "=", "f", "0_", "1_", "0_", "."},
		{". ", "", "id", "a", "=", "a", "."},
		{". ", "010_", "=", "f", "0_", "1_", "0_", "."},
	}, t)
}
