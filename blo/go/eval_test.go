package main

import (
	"strings"
	"testing"
)

func testEval(t *testing.T, label, input, funcName string, args []*Value, expected *Value) {
	tokens := make(chan Token)
	go func() {
		Tokenize("(stdin)", strings.NewReader(input), tokens)
		close(tokens)
	}()
	ast, err := Parse(tokens)
	if err != nil {
		t.Errorf("%s: Unexpected parse error: %s", label, err.Error())
	} else if err := ast.Annotate(); err != nil {
		t.Errorf("%s: Unexpected annotate error: %s", label, err.Error())
	}
	funcDecl := ast.Funcs[funcName]
	if funcDecl == nil {
		t.Errorf("%s: No function %s", label, funcName)
	}
	if len(args) != len(funcDecl.Params) {
		t.Errorf("%s: Function %s needs %d arguments, got %d", label, funcName, len(funcDecl.Params), len(args))
	}
	for i, param := range funcDecl.Params {
		if param.Type.BitSize() != len(args[i].Bits) {
			t.Errorf("%s: Argument %d needs %d bits, got %d", label, i, param.Type.BitSize(), len(args[i].Bits))
		}
		if param.Type.OpaqueSize() != len(args[i].Opaque) {
			t.Errorf("%s: Argument %d needs %d opaques, got %d", label, i, param.Type.OpaqueSize(), len(args[i].Opaque))
		}
	}
	if expected == nil && funcDecl.Type != nil {
		t.Errorf("%s: Expected no result from %s, got type %s", label, funcName, funcDecl.TypeName)
	}
	if expected != nil && funcDecl.Type == nil {
		t.Errorf("%s: Expected result from %s, which returns no value", label, funcName)
	}
	result := EvalFunc(funcDecl, args)
	if expected == nil && result != nil {
		t.Errorf("%s: Unexpected result returned by function %s", label, funcName)
	} else if expected != nil && result == nil {
		t.Errorf("%s: Function %s expected to return a value, returned no value", label, funcName)
	} else if expected != nil && result != nil {
		if len(expected.Bits) != len(result.Bits) {
			t.Errorf("%s: Function %s expected to return %d bits, got %d", label, funcName, len(expected.Bits), len(result.Bits))
		}
		if len(expected.Opaque) != len(result.Opaque) {
			t.Errorf("%s: Function %s expected to return %d opaques, got %d", label, funcName, len(expected.Opaque), len(result.Opaque))
		}
		for i, b := range expected.Bits {
			if b != result.Bits[i] {
				t.Errorf("%s: Result bit %d expected %v, got %v", label, i, b, result.Bits[i])
			}
		}
	}
}

func TestEval(t *testing.T) {
	testEval(t, "empty", `func main() {}`, "main", []*Value{}, nil)

	testEval(t, "id", `
type a {
    a
}
func f(a a) a {
    return a
}`, "f", []*Value{&Value{Bits: []bool{false}}}, &Value{Bits: []bool{false}})

	testEval(t, "id2", `
type a {
    a
}
func f(a a) a {
    return a
}`, "f", []*Value{&Value{Bits: []bool{true}}}, &Value{Bits: []bool{true}})

	testEval(t, "field access", `
type a {
    b b
}
type b {
    b
}
func f(a a) b {
    return a.b
}`, "f", []*Value{&Value{Bits: []bool{true}}}, &Value{Bits: []bool{true}})

	testEval(t, "var", `
type a {
    b b
}
type b {
    b
}
func f(a a) b {
    var b b
    return b
}`, "f", []*Value{&Value{Bits: []bool{true}}}, &Value{Bits: []bool{false}})

	testEval(t, "set/clear", `
type a {
    1, 2
}
func f(a a) a {
    set a.1
    clear a.2
    return a
}`, "f", []*Value{&Value{Bits: []bool{false, true}}}, &Value{Bits: []bool{true, false}})

	testEval(t, "assign", `
type a {
    1, 2
}
func f(a a) a {
    var b a = a
    set a.1
    clear a.2
    return b
}`, "f", []*Value{&Value{Bits: []bool{false, true}}}, &Value{Bits: []bool{true, false}})

	testEval(t, "assign 2", `
type a {
    1, 2
}
type b {
    1, 2 a
}
func f(a a) b {
    var b b
    b.1 = a
    set a.1
    clear a.2
    b.2 = a
    return b
}`, "f", []*Value{&Value{Bits: []bool{false, true}}}, &Value{Bits: []bool{false, true, true, false}})

	testEval(t, "if", `
type a {
    1, 2
}
type b {
    1, 2 a
}
func f(a a) b {
    var b b
    b.1.1 = a.2
    b.1.2 = a.1
    b.2 = a
    if a.1 {
        return b
    }
    clear b.1.1
    return b
}`, "f", []*Value{&Value{Bits: []bool{false, true}}}, &Value{Bits: []bool{false, false, false, true}})

	testEval(t, "for", `
type a {
    1, 2, 3, 4
}
func f(a a) a {
    var b a
    for {
        if a.1 {
            set b.1
            break
        } else if a.2 {
            set a.1
            set b.2
        } else if a.3 {
            set a.2
            set b.3
        } else if a.4 {
            set a.3
            set b.4
        } else {
            break
        }
    }
    return b
}`, "f", []*Value{&Value{Bits: []bool{false, false, false, false}}}, &Value{Bits: []bool{false, false, false, false}})

	testEval(t, "for", `
type a {
    1, 2, 3, 4
}
func f(a a) a {
    var b a
    for {
        if a.1 {
            set b.1
            break
        } else if a.2 {
            set a.1
            set b.2
        } else if a.3 {
            set a.2
            set b.3
        } else if a.4 {
            set a.3
            set b.4
        } else {
            break
        }
    }
    return b
}`, "f", []*Value{&Value{Bits: []bool{false, false, true, false}}}, &Value{Bits: []bool{true, true, true, false}})

	testEval(t, "function call", `
type a {
    1, 2
}
type b {
    1, 2, 3 a
}
func g(a a) {
    if a.1 {
        clear a.1
    } else {
        set a.1
    }
}
func f(a a) b {
    var b b
    b.1 = a
    g(a)
    b.2 = a
    g(b.3)
    return b
}`, "f", []*Value{&Value{Bits: []bool{false, true}}}, &Value{Bits: []bool{false, true, true, true, true, false}})
}
