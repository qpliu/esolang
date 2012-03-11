package main

import (
	"./ast"
	"./bitlist"
	"./tokenize"
	"fmt"
	"os"
	"path/filepath"
)

type args struct {
	srcFiles []string
	fnName   string
	argFiles []string
}

func fnName(filename string) string {
	filename = filepath.Base(filename)
	for i, c := range filename {
		switch c {
		case '.', '_', '0', '1', '=':
			return filename[:i]
		}
	}
	return filename
}

func parseArgs(arglist []string) args {
	dash := 0
	for dash < len(arglist) && arglist[dash] != "-" {
		dash++
	}
	if dash >= len(arglist)-1 {
		return args{arglist[:dash], fnName(arglist[0]), nil}
	}
	return args{arglist[:dash], arglist[dash+1], arglist[dash+2:]}
}

func main() {
	if len(os.Args) == 0 {
		return
	}
	args := parseArgs(os.Args[1:])
	defs, err := ast.Parse(tokenize.Tokenize(args.srcFiles))
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
	stdinArg := bitlist.NewReaderBitlist(os.Stdin)
	defaultArg := stdinArg
	deflist := defs[args.fnName]
	if deflist == nil {
		panic(fmt.Sprintf("%s not defined", args.fnName))
	}
	argCount := len(deflist[0].Parameters)
	fnArgs := make([]bitlist.Bitlist, 0, argCount)
	for i := 0; i < argCount; i++ {
		if i >= len(args.argFiles) {
			fnArgs = append(fnArgs, defaultArg)
		} else if args.argFiles[i] == "-" {
			fnArgs = append(fnArgs, stdinArg)
		} else {
			file, err := os.Open(args.argFiles[i])
			if err != nil {
				panic(err.Error())
			}
			defer file.Close()
			fnArgs = append(fnArgs, bitlist.NewReaderBitlist(file))
		}
		defaultArg = bitlist.NilBitlist
	}
	value := ast.EvalFn(deflist, fnArgs)
	if value == nil {
		panic(fmt.Sprintf("No matching def for %s", args.fnName))
	}
	bitlist.WriteBits(os.Stdout, value)
}
