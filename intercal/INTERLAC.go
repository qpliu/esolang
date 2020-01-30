package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	"./intercal"
)

func usage() {
	fmt.Fprintf(os.Stderr, "Usage: %s [-cg1|-cg2] [-strict] [-llc LLC-CMD] [-cc CC-CMD] [-ll|-o EXEFILE] SRCFILE [SRCFILE...]\n", os.Args[0])
	os.Exit(1)
}

func main() {
	srcFileIndex := 1
	exeFile := ""
	llFlag := false
	strictFlag := false
	cgVersion := 1
	cc := os.Getenv("CC")
	if cc == "" {
		cc = "cc"
	}
	llc := os.Getenv("LLC")
	if llc == "" {
		llc = "llc"
	}
	for {
		if srcFileIndex >= len(os.Args) {
			usage()
		} else if os.Args[srcFileIndex] == "-o" {
			if srcFileIndex+1 >= len(os.Args) {
				usage()
			}
			exeFile = os.Args[srcFileIndex+1]
			srcFileIndex += 2
		} else if os.Args[srcFileIndex] == "-cc" {
			if srcFileIndex+1 >= len(os.Args) {
				usage()
			}
			cc = os.Args[srcFileIndex+1]
			srcFileIndex += 2
		} else if os.Args[srcFileIndex] == "-llc" {
			if srcFileIndex+1 >= len(os.Args) {
				usage()
			}
			llc = os.Args[srcFileIndex+1]
			srcFileIndex += 2
		} else if os.Args[srcFileIndex] == "-ll" {
			llFlag = true
			srcFileIndex++
		} else if os.Args[srcFileIndex] == "-strict" {
			strictFlag = true
			srcFileIndex++
		} else if os.Args[srcFileIndex] == "-cg1" {
			cgVersion = 1
			srcFileIndex++
		} else if os.Args[srcFileIndex] == "-cg2" {
			cgVersion = 2
			srcFileIndex++
		} else {
			break
		}
	}

	if srcFileIndex >= len(os.Args) {
		usage()
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

	if strictFlag {
		intercal.Strict(statements)
	}

	llvmVersion := getLLVMVersion(llc)

	if llFlag {
		if cgVersion == 1 {
			intercal.CodeGen(statements, llvmVersion, os.Stdout)
		} else {
			intercal.CodeGen2(statements, llvmVersion, os.Stdout)
		}
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

		if llvmVersion < 7 {
			if _, err := f.Write([]byte(RUNTIME6)); err != nil {
				fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
				os.Exit(1)
			}
		} else {
			if _, err := f.Write([]byte(RUNTIME7)); err != nil {
				fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
				os.Exit(1)
			}
		}

		if cgVersion == 1 {
			if err := intercal.CodeGen(statements, llvmVersion, f); err != nil {
				fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
				os.Exit(1)
			}
		} else {
			if err := intercal.CodeGen2(statements, llvmVersion, f); err != nil {
				fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
				os.Exit(1)
			}
		}
	}()

	cmd := exec.Command(llc, tmpFile+".ll")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s: %s %s.ll\n", os.Args[0], err.Error(), llc, tmpFile)
		os.Exit(1)
	}
	os.Remove(tmpFile + ".ll")

	if exeFile == "" {
		cmd = exec.Command(cc, "-g", tmpFile+".s")
	} else {
		cmd = exec.Command(cc, "-o", exeFile, "-g", tmpFile+".s")
	}
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s: %s %s.s\n", os.Args[0], err.Error(), cc, tmpFile)
		os.Exit(1)
	}
	os.Remove(tmpFile + ".s")
}

func getLLVMVersion(llc string) int {
	buf := &bytes.Buffer{}
	cmd := exec.Command(llc, "-version")
	cmd.Stdout = buf
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s\n", os.Args[0], err.Error())
		os.Exit(1)
	}
	for {
		l, err := buf.ReadBytes('\n')
		if err != nil {
			return 0
		}
		line := string(l)
		line = strings.Trim(line, " \t\n\r")
		if strings.HasPrefix(line, "LLVM version") {
			line = strings.Trim(line[12:], " \t")
			dot := strings.IndexByte(line, '.')
			if dot < 1 {
				return 0
			}
			version, err := strconv.Atoi(line[:dot])
			if err != nil {
				return 0
			}
			return version

		}
	}
}
