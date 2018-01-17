package main

import (
	"llvm.org/llvm/bindings/go/llvm"
)

func main() {
	// tmp testing
	func() {
		context := llvm.NewContext()
		defer context.Dispose()
		mod, _ := RuntimeDecls(context, "test")
		defer mod.Dispose()
		llvm.VerifyModule(mod, llvm.PrintMessageAction)
		mod.Dump()
	}()
	func() {
		context := llvm.NewContext()
		//defer context.Dispose()
		mod, _ := RuntimeDefs(context, "test")
		defer mod.Dispose()
		llvm.VerifyModule(mod, llvm.PrintMessageAction)
		mod.Dump()
	}()
	func() {
		context := llvm.NewContext()
		defer context.Dispose()
		mod, _ := RuntimeDecls(context, "test")
		defer mod.Dispose()
		llvm.VerifyModule(mod, llvm.PrintMessageAction)
		mod.Dump()
	}()
	func() {
		context := llvm.NewContext()
		//defer context.Dispose()
		mod, rtDecls := RuntimeDefs(context, "test")
		AddDGOLLib(mod, rtDecls, "IO")
		defer mod.Dispose()
		llvm.VerifyModule(mod, llvm.PrintMessageAction)
		mod.Dump()
	}()
}
