package main

import (
	"fmt"
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

	intercal.ListStatements(statements, os.Stderr)

	input := intercal.NewIntercalReader(os.Stdin)
	output := intercal.NewIntercalWriter(os.Stdout)
	state := intercal.NewState(statements)

	if err := state.Run(input, output); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
