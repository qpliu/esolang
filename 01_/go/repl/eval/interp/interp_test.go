package interp

import "testing"

func testCompileParams(label string, tokens []string, expected []param, t *testing.T) {
	params := compileParams(tokens)
	if len(params) != len(expected) {
		t.Errorf("len mismatch: %s", label)
		return
	}
	for i, param := range params {
		if len(param.match) != len(expected[i].match) {
			t.Errorf("param[%d] match len mismatch: %s", i, label)
			return
		}
		for j, bit := range param.match {
			if bit != expected[i].match[j] {
				t.Errorf("param[%d] match[%d] mismatch: %s", i, j, label)
				return
			}
		}
		if param.ptype != expected[i].ptype {
			t.Errorf("param[%d] type mismatch: %s", i, label)
			return
		}
		if param.bind != expected[i].bind {
			t.Errorf("param[%d] name mismatch: %s", i, label)
			return
		}
	}
}

func TestCompileParams(t *testing.T) {
	testCompileParams("empty", nil, nil, t)
	testCompileParams("01", []string{"01"}, []param{
		param{match: []bool{false, true}, ptype: paramIgnore},
	}, t)
	testCompileParams("01_", []string{"01_"}, []param{
		param{match: []bool{false, true}, ptype: paramNil},
	}, t)
	testCompileParams("01.", []string{"01", "."}, []param{
		param{match: []bool{false, true}, ptype: paramIgnore},
	}, t)
	testCompileParams("01a", []string{"01", "a"}, []param{
		param{match: []bool{false, true}, ptype: paramBind, bind: "a"},
	}, t)
	testCompileParams("a", []string{"a"}, []param{
		param{match: nil, ptype: paramBind, bind: "a"},
	}, t)
	testCompileParams("01_a", []string{"01_", "a"}, []param{
		param{match: []bool{false, true}, ptype: paramNil},
		param{match: nil, ptype: paramBind, bind: "a"},
	}, t)
	testCompileParams("01.a", []string{"01", ".", "a"}, []param{
		param{match: []bool{false, true}, ptype: paramIgnore},
		param{match: nil, ptype: paramBind, bind: "a"},
	}, t)
	testCompileParams("01.a_", []string{"01", ".", "a", "_"}, []param{
		param{match: []bool{false, true}, ptype: paramIgnore},
		param{match: nil, ptype: paramBind, bind: "a"},
		param{match: nil, ptype: paramNil},
	}, t)
	testCompileParams("01_.a_", []string{"01_", ".", "a", "_"}, []param{
		param{match: []bool{false, true}, ptype: paramNil},
		param{match: nil, ptype: paramIgnore},
		param{match: nil, ptype: paramBind, bind: "a"},
		param{match: nil, ptype: paramNil},
	}, t)
}
