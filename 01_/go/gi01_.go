package main

import (
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
	end := 0
	for end < len(filename) && filename[end] != '.' {
		end++
	}
	return filename[:end]
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
	defs, err := Parse(Tokenize(args.srcFiles))
	if err != nil {
		fmt.Println(err.Error())
	} else {
		stdinArg := NewReaderBitlist(os.Stdin)
		var defaultArg Bitlist = stdinArg
		deflist := defs[args.fnName]
		if deflist == nil {
			panic(fmt.Sprintf("%s not defined", args.fnName))
		}
		fnArgs := []Bitlist{}
		for i := 0; i < len(deflist[0].Parameters); i++ {
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
				fnArgs = append(fnArgs, NewReaderBitlist(file))
			}
			defaultArg = NilBitlist
		}
		value := EvalFn(deflist, fnArgs)
		if value == nil {
			panic(fmt.Sprintf("No matching def for %s", args.fnName))
		}
		WriteBits(os.Stdout, value)
	}
}
