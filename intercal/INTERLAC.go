package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"./intercal"
)

func main() {
	if len(os.Args) < 2 || (os.Args[1] == "-o" && len(os.Args) < 3) {
		fmt.Fprintf(os.Stderr, "Usage: %s [-ll|-o EXEFILE] SRCFILE [SRCFILE...]\n", os.Args[0])
		os.Exit(1)
	}
	srcFileIndex := 1
	exeFile := ""
	llFlag := false
	if os.Args[1] == "-o" {
		exeFile = os.Args[2]
		srcFileIndex = 3
	} else if os.Args[1] == "-ll" {
		llFlag = true
		srcFileIndex = 2
	}

	tokenizer, err := intercal.NewFileTokenizer(os.Args[srcFileIndex:])
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
		os.Exit(1)
	}

	statements, err := intercal.Parse(tokenizer)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
		os.Exit(1)
	}

	if llFlag {
		intercal.CodeGen(statements, os.Stdout)
		return
	}

	tmpFile := filepath.Join(os.TempDir(), fmt.Sprintf("INTERLAC%d", os.Getpid()))

	func() {
		f, err := os.Create(tmpFile + ".ll")
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
			os.Exit(1)
		}
		defer f.Close()

		if _, err := f.Write([]byte(RUNTIME)); err != nil {
			fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
			os.Exit(1)
		}

		if err := intercal.CodeGen(statements, f); err != nil {
			fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
			os.Exit(1)
		}
	}()

	cmd := exec.Command("llc", tmpFile+".ll")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s: llc %s.ll\n", os.Args[0], err.Error(), tmpFile)
		os.Exit(1)
	}
	os.Remove(tmpFile + ".ll")

	if exeFile == "" {
		cmd = exec.Command("cc", tmpFile+".s")
	} else {
		cmd = exec.Command("cc", "-o", exeFile, tmpFile+".s")
	}
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s: cc %s.s\n", os.Args[0], err.Error(), tmpFile)
		os.Exit(1)
	}
	os.Remove(tmpFile + ".s")
}
