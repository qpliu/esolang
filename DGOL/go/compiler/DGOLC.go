package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"llvm.org/llvm/bindings/go/llvm"
)

func main() {
	if len(os.Args) <= 1 {
		usage()
	}

	srcFiles := []string{}
	objFiles := []string{}
	outFile := ""
	output := ""
	libs := []string{}

	for i := 1; i < len(os.Args); i++ {
		arg := os.Args[i]
		switch arg {
		case "-c", "-ll", "-bc", "-s":
			if output != "" || outFile != "" {
				usage()
			}
			output = arg
			continue
		case "-o":
			i++
			if i >= len(os.Args) || output != "" || outFile != "" {
				usage()
			}
			outFile = os.Args[i]
			continue
		}
		if strings.HasPrefix(arg, "-L=") {
			libs = append(libs, arg[3:])
		} else if filepath.Ext(arg) == ".DGOL" {
			srcFiles = append(srcFiles, arg)
		} else {
			objFiles = append(objFiles, arg)
		}
	}

	dir, err := os.Getwd()
	if err != nil {
		println(err.Error())
		os.Exit(1)
	}

	if output == "" {
		if outFile == "" {
			outFile = "a.out"
		}
		buildExecutable(srcFiles, libs, dir, objFiles, outFile)
	} else {
		llvm.InitializeNativeTarget()
		for _, srcFile := range srcFiles {
			buildObject(srcFile, libs, dir, output)
		}
	}
}

func usage() {
	fmt.Fprintf(os.Stderr, "Usage: %[1]s [-o BINFILE | -c | -s | -ll | -bc] [-L=LIB ...] FILE FILE ...\n\nExample:\n%[1]s -o HELLO -L=IO HELLO.DGOL\n", os.Args[0])
	os.Exit(1)
}

func buildExecutable(srcFiles, libs []string, dir string, objFiles []string, outFile string) {
	args := []string{"-o", outFile}
	args = append(args, objFiles...)
	tmpDir := os.TempDir()
	for i, srcFile := range srcFiles {
		objFile := filepath.Join(tmpDir, fmt.Sprintf("%d.%d.bc", os.Getpid(), i))
		args = append(args, objFile)

		file, err := os.Open(srcFile)
		if err != nil {
			println(err.Error())
			os.Exit(1)
		}
		defer file.Close()

		astModule, err := Parse(srcFile, dir, file)
		if err != nil {
			println(err.Error())
			os.Exit(1)
		}

		context := llvm.NewContext()
		//defer context.Dispose()
		mod := CodeGen(context, astModule, libs)
		//defer mod.Dispose()

		mb := llvm.WriteBitcodeToMemoryBuffer(mod)
		defer mb.Dispose()

		out, err := os.Create(objFile)
		if err != nil {
			println(err.Error())
			os.Exit(1)
		}
		defer out.Close()

		if _, err := out.Write(mb.Bytes()); err != nil {
			println(err.Error())
			os.Exit(1)
		}
	}
	exec.Command("clang", args...).Run()
}

func buildObject(srcFile string, libs []string, dir string, output string) {
	file, err := os.Open(srcFile)
	if err != nil {
		println(err.Error())
		return
	}
	defer file.Close()

	astModule, err := Parse(srcFile, dir, file)
	if err != nil {
		println(err.Error())
		return
	}

	context := llvm.NewContext()
	//defer context.Dispose()
	mod := CodeGen(context, astModule, libs)
	//defer mod.Dispose()

	outFile := srcFile[0 : len(srcFile)-5] // -".DGOL"
	obj := []byte{}
	switch output {
	case "-ll":
		outFile = outFile + ".ll"
		obj = []byte(mod.String())
	case "-bc":
		outFile = outFile + ".bc"
		mb := llvm.WriteBitcodeToMemoryBuffer(mod)
		defer mb.Dispose()
		obj = mb.Bytes()
	case "-s":
		outFile = outFile + ".s"
		obj = buildObj(mod, llvm.AssemblyFile)
	case "-c":
		outFile = outFile + ".o"
		obj = buildObj(mod, llvm.ObjectFile)
	}

	if len(obj) == 0 {
		return
	}

	out, err := os.Create(outFile)
	if err != nil {
		println(err.Error())
		return
	}
	defer out.Close()

	if _, err := out.Write(obj); err != nil {
		println(err.Error())
		return
	}
}

func buildObj(mod llvm.Module, codeGenFileType llvm.CodeGenFileType) []byte {
	target := llvm.FirstTarget()
	machine := target.CreateTargetMachine(llvm.DefaultTargetTriple(), "", "", llvm.CodeGenLevelDefault, llvm.RelocDefault, llvm.CodeModelDefault)
	defer machine.Dispose()
	mb, err := machine.EmitToMemoryBuffer(mod, codeGenFileType)
	if err != nil {
		println(err.Error())
		return nil
	}
	defer mb.Dispose()
	return mb.Bytes()
}
