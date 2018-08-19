package main

import (
	"os"
)

func main() {
	if len(os.Args) < 2 {
		println("Usage:", os.Args[0], "SRCFILE [SRCFILE...]")
		os.Exit(1)
	}
	tokenizer, err := NewFileTokenizer(os.Args[1:])
	if err != nil {
		println(err.Error())
		os.Exit(1)
	}

	statements, err := Parse(tokenizer)
	if err != nil {
		println(err.Error())
		os.Exit(1)
	}

	ListStatements(statements, os.Stdout)

	input := NewIntercalReader(os.Stdin)

	state := NewState(statements)

	if err := state.Run(input, os.Stdout); err != nil {
		println(err.Error())
		os.Exit(1)
	}
}
