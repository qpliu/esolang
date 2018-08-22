package main

import (
	"os"

	"./intercal"
)

func main() {
	if len(os.Args) < 2 || (os.Args[1] == "-o" && len(os.Args) < 3) {
		println("Usage:", os.Args[0], "[-o EXEFILE] SRCFILE [SRCFILE...]")
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
}
