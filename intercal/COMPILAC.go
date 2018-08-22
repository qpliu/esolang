package main

import (
	"os"

	"./intercal"
)

func main() {
	if len(os.Args) < 2 {
		println("Usage:", os.Args[0], "SRCFILE [SRCFILE...]")
		os.Exit(1)
	}
	tokenizer, err := intercal.NewFileTokenizer(os.Args[1:])
	if err != nil {
		println(err.Error())
		os.Exit(1)
	}

	statements, err := intercal.Parse(tokenizer)
	if err != nil {
		println(err.Error())
		os.Exit(1)
	}

	intercal.ListStatements(statements, os.Stdout)

	input := intercal.NewIntercalReader(os.Stdin)

	state := intercal.NewState(statements)

	if err := state.Run(input, os.Stdout); err != nil {
		println(err.Error())
		os.Exit(1)
	}
}
