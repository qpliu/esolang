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

func exprEq(e1, e2 Expr) bool {
	switch e1.(type) {
	case literalExpr:
		e, ok := e2.(literalExpr)
		return ok && e1 == e
	case argExpr:
		e, ok := e2.(argExpr)
		return ok && e1 == e
	case concatExpr:
		e, ok := e2.(concatExpr)
		ee := e1.(concatExpr)
		return ok && exprEq(ee[0], e[0]) && exprEq(ee[1], e[1])
	case funcallExpr:
		e, ok := e2.(funcallExpr)
		ee := e1.(funcallExpr)
		if !ok || ee.def != e.def || len(ee.args) != len(e.args) {
			return false
		}
		for i, arg := range ee.args {
			if !exprEq(arg, e.args[i]) {
				return false
			}
		}
		return true
	default:
		return false
	}
}
