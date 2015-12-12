package main

import (
	"errors"
	"os"
)

func AnnotateRuntime(ast *Ast) error {
	for _, funcDecl := range ast.Funcs {
		if funcDecl.Imported {
			if runtime, ok := runtimeFuncs[funcDecl.Name]; !ok {
				return errors.New(funcDecl.Location.String() + ": import unknown function '" + funcDecl.Name + "'")
			} else {
				funcDecl.Runtime = runtime
			}
		}
	}
	return nil
}

var runtimeFuncs map[string]func(*Func, []*Value) *Value = map[string]func(*Func, []*Value) *Value{
	"getByte": runtimeGetByte,
	"putByte": runtimePutByte,
}

func runtimeGetByte(funcDecl *Func, args []*Value) *Value {
	var buf [1]byte
	if _, err := os.Stdin.Read(buf[:]); err != nil {
		if len(args) > 0 && len(args[0].Bits) > 8 {
			args[0].Bits[8] = true
		}
	} else {
		if len(args) > 0 {
			if len(args[0].Bits) > 8 {
				args[0].Bits[8] = false
			}
			for i := 0; i < 8 && i < len(args[0].Bits); i++ {
				args[0].Bits[i] = (buf[0]>>uint(i))&1 != 0
			}
		}
	}
	if funcDecl.Type == nil {
		return nil
	} else {
		return NewValue(funcDecl.Type)
	}
}

func runtimePutByte(funcDecl *Func, args []*Value) *Value {
	var buf [1]byte
	if len(args) > 0 {
		for i := 0; i < 8 && i < len(args[0].Bits); i++ {
			if args[0].Bits[i] {
				buf[0] |= 1 << uint(i)
			}
		}
	}
	if _, err := os.Stdout.Write(buf[:]); err != nil {
		panic(err.Error())
	}
	if funcDecl.Type == nil {
		return nil
	} else {
		return NewValue(funcDecl.Type)
	}
}
