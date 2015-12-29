package main

import (
	"os"
	"strings"
)

func main() {
	var srcs []string
	var outflag, outfile string
	f := func(tokens <-chan Token, outflag, outfile string) error {
		return interp(tokens)
	}
	if len(os.Args) > 2 && os.Args[1] == "-o" {
		f = compile
		outflag = "-o"
		outfile = os.Args[2]
		srcs = os.Args[3:]
	} else if len(os.Args) > 2 && os.Args[1] == "-c" {
		f = compile
		outflag = "-c"
		srcs = os.Args[2:]
	} else if len(os.Args) > 2 && os.Args[1] == "-S" {
		f = compile
		outflag = "-S"
		srcs = os.Args[2:]
	} else if len(os.Args) > 0 && (os.Args[0] == "compile" || strings.HasSuffix(os.Args[0], "/compile")) {
		f = compile
		srcs = os.Args[1:]
	} else if len(os.Args) > 0 {
		srcs = os.Args[1:]
	}

	tokens := make(chan Token)
	go func() {
		for _, file := range srcs {
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
	if err := f(tokens, outflag, outfile); err != nil {
		os.Stderr.WriteString(err.Error() + "\n")
		os.Exit(1)
	}
}
