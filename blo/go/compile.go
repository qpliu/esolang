package main

import (
	"errors"
	"io/ioutil"
	"os"
	"os/exec"
)

func compile(tokens <-chan Token) error {
	var ast *Ast
	if _ast, err := Parse(tokens); err != nil {
		return err
	} else {
		ast = _ast
	}
	if err := ast.Annotate(); err != nil {
		return err
	}
	mainFunc := ast.Funcs["main"]
	if mainFunc == nil || len(mainFunc.Params) != 0 {
		return errors.New("No 'main' function with zero arguments")
	}
	var llfile *os.File
	if _llfile, err := ioutil.TempFile("", "blo"); err != nil {
		return err
	} else {
		llfile = _llfile
	}
	defer os.Remove(llfile.Name())
	println("file=" + llfile.Name())
	if err := LLVMCodeGen(ast, llfile); err != nil {
		return err
	}
	llfile.Close()
	llcmd := exec.Command("llc", "-filetype=obj", llfile.Name())
	if err := llcmd.Run(); err != nil {
		return err
	}
	defer os.Remove(llfile.Name() + ".o")
	cccmd := exec.Command("gcc", llfile.Name()+".o")
	if err := cccmd.Run(); err != nil {
		return err
	}
	return nil
}
