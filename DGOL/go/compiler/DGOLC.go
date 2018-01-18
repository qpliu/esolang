package main

import (
	"os"
	"path/filepath"

	"llvm.org/llvm/bindings/go/llvm"
)

func main() {
	dir, err := os.Getwd()
	if err != nil {
		println(err.Error())

	}
	// tmp testing
	for _, arg := range os.Args {
		if filepath.Ext(arg) == ".DGOL" {
			if file, err := os.Open(arg); err != nil {
				println(err.Error())
			} else if astModule, err := Parse(arg, dir, file); err != nil {
				file.Close()
				println(err.Error())
			} else {
				file.Close()
				context := llvm.NewContext()
				//defer context.Dispose()
				mod := CodeGen(context, astModule, []string{})
				//defer mod.Dispose()
				llvm.VerifyModule(mod, llvm.PrintMessageAction)
				mod.Dump()
			}
		}
	}
}
