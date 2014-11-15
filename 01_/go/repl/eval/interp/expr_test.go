package interp

import (
	"strconv"
	"testing"
)

func TestExpr(t *testing.T) {
	defs := make(map[string]*Def)
	id := &Def{
		name:   "id",
		arity:  1,
		bodies: [][2][]string{{{"x"}, {"x"}}},
	}
	defs["id"] = id
	if err := id.compile(defs); err != nil {
		t.Errorf("id: compile error: %s", err.Error())
		return
	}
	if len(id.compiled) != 1 {
		t.Errorf("len(id.compile) != 1")
		return
	}
	if err := id.compiled[0].compileErr; err != nil {
		t.Errorf("id: compile error: compileErr=%s", err.Error())
		return
	}
	if len(id.compiled) != 1 || len(id.compiled[0].params) != 1 || len(id.compiled[0].params[0].match) != 0 || id.compiled[0].params[0].ptype != paramBind || id.compiled[0].body != argExpr(0) {
		t.Errorf("id: compile error")
		return
	}
	concat := &Def{
		name:   "concat",
		arity:  2,
		bodies: [][2][]string{{{"a", "b"}, {"a", "b"}}},
	}
	defs["concat"] = concat
	if err := concat.compile(defs); err != nil {
		t.Errorf("concat: compile error: %s", err.Error())
		return
	}
	if len(concat.compiled) != 1 {
		t.Errorf("len(concat.compile) != 1")
		return
	}
	if err := concat.compiled[0].compileErr; err != nil {
		t.Errorf("concat: compile error: compileErr=%s", err.Error())
		return
	}
	if len(concat.compiled) != 1 || len(concat.compiled[0].params) != 2 || len(concat.compiled[0].params[0].match) != 0 || concat.compiled[0].params[0].ptype != paramBind || len(concat.compiled[0].params[1].match) != 0 || concat.compiled[0].params[1].ptype != paramBind || !exprEq(concat.compiled[0].body, concatExpr{argExpr(0), argExpr(1)}) {
		t.Errorf("concat: compile error")
		return
	}
	expr := funcallExpr{
		def: concat,
		args: []Expr{
			funcallExpr{id, []Expr{literalExpr("0_")}},
			literalExpr("1_"),
		},
	}
	val := EvalExpr(defs, expr, nil)
	if !checkVal("1", val, vthunk, expr, 0, defs, t) {
		return
	}
	if !checkVal("2:preliminary", val, vthunk, concatExpr{argExpr(0), argExpr(1)}, 2, nil, t) {
		return
	}
	if !checkVal("2:bindings[0]", val.bindings[0], vthunk, funcallExpr{id, []Expr{literalExpr("0_")}}, 0, nil, t) {
		return
	}
	if !checkVal("2:bindings[1]", val.bindings[1], vthunk, literalExpr("1_"), 0, nil, t) {
		return
	}
	if !checkVal("2", val, vthunk, concatExpr{argExpr(0), argExpr(1)}, 2, defs, t) {
		return
	}
	if !checkVal("3:preliminary", val, vthunk, concatExpr{argExpr(0), argExpr(1)}, 2, nil, t) {
		return
	}
	if !checkVal("3:bindings[0]", val.bindings[0], vthunk, argExpr(0), 1, nil, t) {
		return
	}
	if !checkVal("3:bindings[1]", val.bindings[1], vthunk, literalExpr("1_"), 0, nil, t) {
		return
	}
	if !checkVal("3", val, vthunk, concatExpr{argExpr(0), argExpr(1)}, 2, defs, t) {
		return
	}
	if !checkVal("4", val, v0, nil, 0, nil, t) {
		return
	}
	val = val.next
	if !checkVal("5:preliminary", val, vthunk, concatExpr{literalExpr("_"), argExpr(1)}, 2, nil, t) {
		return
	}
	if !checkVal("5:bindings[0]", val.bindings[0], v0, nil, 0, nil, t) {
		return
	}
	if !checkVal("5:bindings[0].next", val.bindings[0].next, vthunk, literalExpr("_"), 0, nil, t) {
		return
	}
	if !checkVal("5:bindings[1]", val.bindings[1], vthunk, literalExpr("1_"), 0, nil, t) {
		return
	}
	if !checkVal("5", val, vthunk, concatExpr{literalExpr("_"), argExpr(1)}, 2, defs, t) {
		return
	}
	if !checkVal("6.preliminary", val, vthunk, argExpr(1), 2, nil, t) {
		return
	}
	if !checkVal("6:bindings[0]", val.bindings[0], v0, nil, 0, nil, t) {
		return
	}
}

func checkVal(label string, val *Value, vtype valType, expr Expr, nbindings int, defs map[string]*Def, t *testing.T) bool {
	if val == nil {
		t.Errorf("val == nil: %s", label)
		return false
	}
	if val.val != vtype {
		t.Errorf("val.val != vtype: %s", label)
		return false
	}
	if expr != nil && !exprEq(val.expr, expr) {
		t.Errorf("val.expr != expr: %s: %s", exprToStr(val.expr), label)
		return false
	}
	if len(val.bindings) != nbindings {
		t.Errorf("len(val.bindings) != expr: %s", label)
		return false
	}
	if defs != nil {
		if err := val.expr.Eval(val, defs); err != nil {
			t.Errorf("eval error: %s: %s", label, err.Error())
			return false
		}
	}
	return true
}

func exprToStr(expr Expr) string {
	switch expr.(type) {
	case literalExpr:
		return "L" + string(expr.(literalExpr))
	case argExpr:
		return "A" + strconv.Itoa(int(expr.(argExpr)))
	case concatExpr:
		expr := expr.(concatExpr)
		return "C(" + exprToStr(expr[0]) + "," + exprToStr(expr[1]) + ")"
	case funcallExpr:
		expr := expr.(funcallExpr)
		str := "F" + expr.def.name + "("
		sep := ""
		for _, arg := range expr.args {
			str += sep + exprToStr(arg)
		}
		return str + ")"
	default:
		return "Unknown"
	}
}
