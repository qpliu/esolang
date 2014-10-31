package main

import (
	"./eval"
	"./repl"
	"./tokenize"
	"fmt"
	"io"
	"os"
)

const (
	version = "0.1"
)

func main() {
	io.WriteString(os.Stdout, fmt.Sprintf("01_ interpreter, version %s\nType \".?\" for help\n", version))
	repl.Repl(os.Stdin, os.Stdout, &eval.State{}, &tokenize.Tokenizer{})
}
