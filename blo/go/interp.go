package main

import (
	"errors"
)

func interp(tokens <-chan Token) error {
	var ast *Ast
	if _ast, err := Parse(tokens); err != nil {
		return err
	} else {
		ast = _ast
	}
	if err := ast.Annotate(); err != nil {
		return err
	}
	if err := AnnotateRuntime(ast); err != nil {
		return err
	}
	mainFunc := ast.Funcs["main"]
	if mainFunc == nil || len(mainFunc.Params) != 0 {
		return errors.New("No 'main' function with zero arguments")
	}
	EvalFunc(mainFunc, []*Value{})
	return nil
}
