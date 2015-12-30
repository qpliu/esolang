package main

import (
	"os"
	"strings"
)

func main() {
	if len(os.Args) <= 1 {
		progName := "blo"
		if len(os.Args) > 0 {
			progName = os.Args[0]
		}
		os.Stderr.WriteString("Usage:\n")
		os.Stderr.WriteString(progName)
		os.Stderr.WriteString(" SRCFILE ...\n")
		os.Stderr.WriteString(progName)
		os.Stderr.WriteString(" -S SRCFILE ...\n")
		os.Stderr.WriteString(progName)
		os.Stderr.WriteString(" -s SRCFILE ...\n")
		os.Stderr.WriteString(progName)
		os.Stderr.WriteString(" -o EXECUTABLEFILE SRCFILE ...\n")
		os.Exit(1)
	}

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
	} else if len(os.Args) > 2 && os.Args[1] == "-s" {
		f = compile
		outflag = "-s"
		srcs = os.Args[2:]
	} else if len(os.Args) > 2 && os.Args[1] == "-S" {
		f = compile
		outflag = "-S"
		srcs = os.Args[2:]
	} else if os.Args[0] == "compile" || strings.HasSuffix(os.Args[0], "/compile") {
		f = compile
		srcs = os.Args[1:]
	} else {
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
