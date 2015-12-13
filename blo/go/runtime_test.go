package main

import (
	"testing"
)

func testRuntime(t *testing.T, label string, funcDecl *Func, args []*Value, expected *Value) {
}

func TestRuntime(t *testing.T) {
	ast, err := testCompile(`
import type stack {}
import func pushStack(stack stack, bit bit)
import func popStack(stack stack) bit
import func isEmptyStack(stack stack) bit
type bit { b }
type result { 1, 2, 3, 4, 5, 6, 7 }

func test() result {
    var result result
    var b bit
    var stack stack
    if isEmptyStack(stack).b {
    } else {
        set result.1
        return result
    }
    pushStack(stack, b)
    if isEmptyStack(stack).b {
        set result.2
        return result
    }
    set b.b
    pushStack(stack, b)
    if isEmptyStack(stack).b {
        set result.3
        return result
    }
    if popStack(stack).b {
    } else {
        set result.4
        return result
    }
    if popStack(stack).b {
        set result.5
        return result
    }
    if isEmptyStack(stack).b {
    } else {
        set result.6
        return result
    }
    set result.7
    return result
}
`)
	if err != nil {
		t.Errorf("compile error: %s", err.Error())
	}

	testRuntime(t, "test", ast.Funcs["test"], []*Value{}, &Value{Bits: []bool{false, false, false, false, false, false, true}})
}
