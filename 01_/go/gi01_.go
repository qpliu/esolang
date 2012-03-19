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

func makeArgs(argCount int, argFiles []string) []bitlist.Bitlist {
	fnArgs := make([]bitlist.Bitlist, 0, argCount)
	stdinArg := bitlist.NewReaderBitlist(os.Stdin)
	for i := 0; i < argCount; i++ {
		switch {
		case i >= len(argFiles) && i > 0:
			fnArgs = append(fnArgs, bitlist.NilBitlist)
		case i >= len(argFiles) || argFiles[i] == "-":
			fnArgs = append(fnArgs, stdinArg)
		default:
			file, err := os.Open(argFiles[i])
			if err != nil {
				fmt.Fprintln(os.Stderr, err.Error())
				os.Exit(1)
			}
			fnArgs = append(fnArgs, bitlist.NewReaderBitlist(file))
		}
	}
	return fnArgs
}

func eval(fnName string, deflist []*ast.Def, args []bitlist.Bitlist) bitlist.Bitlist {
	value := ast.EvalFn(deflist, args)
	if value == nil {
		panic(fmt.Sprintf("No matching def for %s", fnName))
	}
	return value
}

func main() {
	if len(os.Args) < 2 {
		progName := "gi01_"
		if len(os.Args) > 0 {
			progName = os.Args[0]
		}
		fmt.Fprintf(os.Stderr, "Usage: %s FILENAME ... [- FUNCTION [FILENAME ...]]\n", progName)
		return
	}
	args := parseArgs(os.Args[1:])
	defs, err := ast.Parse(tokenize.Tokenize(args.srcFiles))
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
	deflist := defs[args.fnName]
	if deflist == nil {
		fmt.Fprintf(os.Stderr, "%s not defined\n", args.fnName)
		os.Exit(1)
	}
	bitlist.WriteBits(os.Stdout, eval(args.fnName, deflist, makeArgs(len(deflist[0].Parameters), args.argFiles)))
}
