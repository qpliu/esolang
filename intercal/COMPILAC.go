package main

import (
	"fmt"
	"os"

	"./intercal"
)

func usage() {
	println("Usage:", os.Args[0], "[-strict] SRCFILE [SRCFILE...]")
	os.Exit(1)
}

func main() {
	srcFileIndex := 1
	strictFlag := false
	for {
		if srcFileIndex >= len(os.Args) {
			usage()
		} else if os.Args[srcFileIndex] == "-strict" {
			strictFlag = true
			srcFileIndex++
		} else {
			break
		}
	}

	tokenizer, err := intercal.NewFileTokenizer(os.Args[srcFileIndex:])
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
		os.Exit(1)
	}

	statements, err := intercal.Parse(tokenizer)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
		os.Exit(1)
	}

	if strictFlag {
		intercal.Strict(statements)
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
