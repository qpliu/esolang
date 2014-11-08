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

func testCompileExpr(label string, tokens []string, defs map[string]*Def, params []param, expected Expr, t *testing.T) {
	expr, err := compileExpr(tokens, defs, params)
	if err != nil {
		if expected != nil {
			t.Errorf("unexpected compile error: %s: %s", err.Error(), label)
		}
	} else if expected == nil {
		t.Errorf("did not get expected compile error: %s", label)
	} else if !exprEq(expr, expected) {
		t.Errorf("unexpected compile result: %s", label)
	}
}

func TestCompileExpr(t *testing.T) {
	defs := make(map[string]*Def)
	testCompileExpr("_", []string{"_"}, defs, nil, literalExpr("_"), t)
	testCompileExpr("__", []string{"_", "_"}, defs, nil, concatExpr{literalExpr("_"), literalExpr("_")}, t)
	testCompileExpr("___", []string{"_", "_", "_"}, defs, nil, concatExpr{literalExpr("_"), concatExpr{literalExpr("_"), literalExpr("_")}}, t)
	params := []param{
		param{match: []bool{false, true}, ptype: paramNil},
		param{match: nil, ptype: paramBind, bind: "p"},
		param{match: nil, ptype: paramIgnore},
		param{match: nil, ptype: paramBind, bind: "p"},
	}
	testCompileExpr("duplicate parameter _", []string{"_"}, defs, params, nil, t)
	testCompileExpr("duplicate parameter a", []string{"a"}, defs, params, nil, t)
	testCompileExpr("duplicate parameter p", []string{"p"}, defs, params, nil, t)
	params = []param{
		param{match: []bool{false, true}, ptype: paramNil},
		param{match: nil, ptype: paramBind, bind: "p"},
		param{match: nil, ptype: paramIgnore},
		param{match: nil, ptype: paramBind, bind: "q"},
	}
	testCompileExpr("f", []string{"f"}, defs, params, nil, t)
	testCompileExpr("p", []string{"p"}, defs, params, argExpr(0), t)
	testCompileExpr("p q", []string{"p", "q"}, defs, params, concatExpr{argExpr(0), argExpr(1)}, t)
	deff := &Def{name: "f", arity: 0}
	defg := &Def{name: "g", arity: 2}
	defs["f"] = deff
	defs["g"] = defg
	testCompileExpr("f", []string{"f"}, defs, params, funcallExpr{def: deff}, t)
	testCompileExpr("g", []string{"g"}, defs, params, nil, t)
	testCompileExpr("g_q", []string{"g", "_", "q"}, defs, params, funcallExpr{def: defg, args: []Expr{literalExpr("_"), argExpr(1)}}, t)
	testCompileExpr("g g f_q f", []string{"g", "g", "f", "_", "q", "f"}, defs, params, concatExpr{funcallExpr{def: defg, args: []Expr{funcallExpr{def: defg, args: []Expr{funcallExpr{def: deff}, literalExpr("_")}}, argExpr(1)}}, funcallExpr{def: deff}}, t)
}

func testEvalExpr(label string, expr Expr, defs map[string]*Def, expected []bool, t *testing.T) {
	eq, err := valueEq(expected, EvalExpr(defs, expr, nil), defs)
	if err != nil {
		t.Errorf("evaluation error: %s: %s", err.Error(), label)
	} else if !eq {
		t.Errorf("testEvalExpr failed: %s", label)
	}
}

func TestEvalExpr(t *testing.T) {
	defs := make(map[string]*Def)
	id := &Def{
		name:   "id",
		arity:  1,
		bodies: [][2][]string{{{"x"}, {"x"}}},
	}
	defs["id"] = id
	concat := &Def{
		name:   "concat",
		arity:  2,
		bodies: [][2][]string{{{"a", "b"}, {"a", "b"}}},
	}
	defs["concat"] = concat
	xor := &Def{
		name:  "xor",
		arity: 2,
		bodies: [][2][]string{
			{{"_", "b"}, {"b"}},
			{{"a", "_"}, {"a"}},
			{{"0", "a", "0", "b"}, {"0", "xor", "a", "b"}},
			{{"0", "a", "1", "b"}, {"1", "xor", "a", "b"}},
			{{"1", "a", "0", "b"}, {"1", "xor", "a", "b"}},
			{{"1", "a", "1", "b"}, {"0", "xor", "a", "b"}},
		},
	}
	defs["xor"] = xor
	testEvalExpr("01_", literalExpr("01_"), defs, []bool{false, true}, t)
	testEvalExpr("id 01_", funcallExpr{def: id, args: []Expr{literalExpr("01_")}}, defs, []bool{false, true}, t)
	testEvalExpr("concat 01_ 01_", funcallExpr{def: concat, args: []Expr{literalExpr("01_"), literalExpr("01_")}}, defs, []bool{false, true, false, true}, t)
	testEvalExpr("xor _ _", funcallExpr{def: xor, args: []Expr{literalExpr("_"), literalExpr("_")}}, defs, nil, t)
	testEvalExpr("xor 0101_ 011001_", funcallExpr{def: xor, args: []Expr{literalExpr("0101_"), literalExpr("011001_")}}, defs, []bool{false, false, true, true, false, true}, t)
	testEvalExpr("xor 010101_ 0110_", funcallExpr{def: xor, args: []Expr{literalExpr("0101_"), literalExpr("011001_")}}, defs, []bool{false, false, true, true, false, true}, t)
	testEvalExpr("concat id 0_ 1_", funcallExpr{def: concat, args: []Expr{funcallExpr{def: id, args: []Expr{literalExpr("0_")}}, literalExpr("1_")}}, defs, []bool{false, true}, t)
}

func exprEq(e1, e2 Expr) bool {
	switch e1.(type) {
	case literalExpr:
		e2, ok := e2.(literalExpr)
		return ok && e1 == e2
	case argExpr:
		e2, ok := e2.(argExpr)
		return ok && e1 == e2
	case concatExpr:
		e1 := e1.(concatExpr)
		e2, ok := e2.(concatExpr)
		return ok && exprEq(e1[0], e2[0]) && exprEq(e1[1], e2[1])
	case funcallExpr:
		e1 := e1.(funcallExpr)
		e2, ok := e2.(funcallExpr)
		if !ok || e1.def != e2.def || len(e1.args) != len(e2.args) {
			return false
		}
		for i, arg := range e1.args {
			if !exprEq(arg, e2.args[i]) {
				return false
			}
		}
		return true
	default:
		return false
	}
}

func valueEq(bits []bool, val *Value, defs map[string]*Def) (bool, error) {
	if val == nil {
		return len(bits) == 0, nil
	}
	for _, bit := range bits {
		b, v, err := val.Force(defs)
		if err != nil || b != bit || val == nil {
			return false, err
		}
		val = v
	}
	_, v, err := val.Force(defs)
	return v == nil && err == nil, err
}
