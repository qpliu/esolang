package main

import (
	"os"
)

func main() {
	tokens := make(chan Token)
	go func() {
		for _, file := range os.Args[1:] {
			if f, err := os.Open(file); err != nil {
				os.Stderr.WriteString(err.Error())
				os.Exit(1)
			} else {
				defer f.Close()
				Tokenize(file, f, tokens)
			}
		}
		close(tokens)
	}()
	ast, err := Parse(tokens)
	if err != nil {
		os.Stderr.WriteString(err.Error())
		os.Exit(1)
	} else if err := ast.Annotate(); err != nil {
		os.Stderr.WriteString(err.Error())
		os.Exit(1)
	} else if err := AnnotateRuntime(ast); err != nil {
		os.Stderr.WriteString(err.Error())
		os.Exit(1)
	}
	mainFunc := ast.Funcs["main"]
	if mainFunc == nil || len(mainFunc.Params) != 0 {
		os.Stderr.WriteString("No 'main' function with zero arguments")
		os.Exit(1)
	}
	EvalFunc(mainFunc, []*Value{})
}
