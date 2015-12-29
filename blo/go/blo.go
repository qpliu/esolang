package main

import (
	"os"
	"strings"
)

func main() {
	tokens := make(chan Token)
	go func() {
		for _, file := range os.Args[1:] {
			if f, err := os.Open(file); err != nil {
				os.Stderr.WriteString(err.Error() + "\n")
				os.Exit(1)
			} else {
				defer f.Close()
				Tokenize(file, f, tokens)
			}
		}
		close(tokens)
	}()

	f := interp
	if os.Args[0] == "compile" || strings.HasSuffix(os.Args[0], "/compile") {
		f = compile
	}
	if err := f(tokens); err != nil {
		os.Stderr.WriteString(err.Error() + "\n")
		os.Exit(1)
	}
}
