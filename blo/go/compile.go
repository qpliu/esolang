package main

import (
	"errors"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"sync"
)

func compile(tokens <-chan Token, outflag, outfile string) error {
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
	if outflag == "-S" {
		return LLVMCodeGen(ast, os.Stdout)
	} else if outflag == "-s" {
		var waitGroup sync.WaitGroup
		llcmd := exec.Command("llc", "-filetype=asm")
		out, err := llcmd.StdinPipe()
		if err != nil {
			return err
		}
		if in, err := llcmd.StdoutPipe(); err != nil {
			return err
		} else {
			waitGroup.Add(1)
			go func() {
				defer waitGroup.Done()
				io.Copy(os.Stdout, in)
			}()
		}
		if in, err := llcmd.StderrPipe(); err != nil {
			return err
		} else {
			waitGroup.Add(1)
			go func() {
				defer waitGroup.Done()
				io.Copy(os.Stderr, in)
			}()
		}
		if err := llcmd.Start(); err != nil {
			return err
		}
		if err := LLVMCodeGen(ast, out); err != nil {
			return err
		}
		out.Close()
		if err := llcmd.Wait(); err != nil {
			return err
		}
		waitGroup.Wait()
		return nil
	}
	var llfile *os.File
	if _llfile, err := ioutil.TempFile("", "blo"); err != nil {
		return err
	} else {
		llfile = _llfile
	}
	defer os.Remove(llfile.Name())
	if err := LLVMCodeGen(ast, llfile); err != nil {
		return err
	}
	llfile.Close()
	llcmd := exec.Command("llc", "-filetype=obj", llfile.Name())
	if err := llcmd.Run(); err != nil {
		return err
	}
	defer os.Remove(llfile.Name() + ".o")
	var ccargs []string
	if outflag == "-o" {
		ccargs = []string{"-o", outfile}
	}
	ccargs = append(ccargs, llfile.Name()+".o")
	cccmd := exec.Command("gcc", ccargs...)
	if err := cccmd.Run(); err != nil {
		return err
	}
	return nil
}
