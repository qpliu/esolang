package main

import (
	"fmt"
	"os"

	"./intercal"
)

func main() {
	if len(os.Args) < 2 || (os.Args[1] == "-o" && len(os.Args) < 3) {
		fmt.Fprintf(os.Stderr, "Usage: %s [-o EXEFILE] SRCFILE [SRCFILE...]\n", os.Args[0])
		os.Exit(1)
	}
	srcFileIndex := 1
	exeFile := ""
	if os.Args[1] == "-o" {
		exeFile = os.Args[2]
		srcFileIndex = 3
	}

	tokenizer, err := intercal.NewFileTokenizer(os.Args[srcFileIndex:])
	if err != nil {
		println(err.Error())
		os.Exit(1)
	}

	statements, err := intercal.Parse(tokenizer)
	if err != nil {
		println(err.Error())
		os.Exit(1)
	}

	//...
	_, _ = statements, exeFile

	if _, err := os.Stdout.Write([]byte(RUNTIME)); err != nil {
		return err
	}

	intercal.CodeGen(statements, os.Stdout)
}
