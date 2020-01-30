package intercal

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

func testCodeGen(t *testing.T, srcFile string, llvmVersion int, cgVersion int) {
	outFile := filepath.Join("testdata", srcFile) + ".codegen.out"
	if _, err := os.Stat(outFile); err != nil {
		return
	}

	tokenizer, err := NewFileTokenizer([]string{filepath.Join("testdata", srcFile)})
	if err != nil {
		t.Errorf("parse %s: %s", srcFile, err.Error())
		return
	}
	statements, err := Parse(tokenizer)
	if err != nil {
		t.Errorf("parse %s: %s", srcFile, err.Error())
		return
	}

	tmpFile := filepath.Join(os.TempDir(), fmt.Sprintf("codegen_test.%s.cg%d.%d", filepath.Base(srcFile), cgVersion, os.Getpid()))

	if err := func() error {
		f, err := os.Create(tmpFile + ".ll")
		if err != nil {
			return err
		}
		defer f.Close()
		if cgVersion == 1 {
			return CodeGen(statements, llvmVersion, f)
		} else {
			return CodeGen2(statements, llvmVersion, f)
		}
	}(); err != nil {
		t.Errorf("codegen %s: %s", srcFile, err.Error())
		return
	}

	if testing.Short() {
		os.Remove(tmpFile)
		return
	}

	cmd := fmt.Sprintf("cat runtime-common.ll runtime-6.ll %s.ll | llc > %s.s", tmpFile, tmpFile)
	if err := exec.Command("sh", "-c", cmd).Run(); err != nil {
		t.Errorf("%s", cmd)
		return
	}

	cmd = fmt.Sprintf("cc -o %s %s.s", tmpFile, tmpFile)
	if err := exec.Command("sh", "-c", cmd).Run(); err != nil {
		t.Errorf("%s", cmd)
		return
	}

	inFile := filepath.Join("testdata", srcFile) + ".in"
	if _, err := os.Stat(inFile); err != nil {
		cmd = fmt.Sprintf("%s 2>&1 | diff -q %s -", tmpFile, outFile)
	} else {
		cmd = fmt.Sprintf("%s < %s 2>&1 | diff -q %s -", tmpFile, inFile, outFile)
	}

	if err := exec.Command("sh", "-c", cmd).Run(); err != nil {
		t.Errorf("%s", cmd)
		return
	}

	os.Remove(tmpFile)
	os.Remove(tmpFile + ".ll")
	os.Remove(tmpFile + ".s")
}

func TestCodeGen(t *testing.T) {
	llvmVersion := 5

	f, err := os.Open("testdata")
	if err != nil {
		t.Error(err.Error())
		return
	}
	defer f.Close()
	names, err := f.Readdirnames(0)
	if err != nil {
		t.Error(err.Error())
		return
	}
	for _, name := range names {
		if filepath.Ext(name) == ".i" {
			const cgVersion1 = 1
			const cgVersion2 = 2
			testCodeGen(t, name, llvmVersion, cgVersion1)
			testCodeGen(t, name, llvmVersion, cgVersion2)
		}
	}
}

func getLLVMVersion(t *testing.T) int {
	buf := &bytes.Buffer{}
	cmd := exec.Command("llc", "-version")
	cmd.Stdout = buf
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Error(err.Error())
		return 0
	}
	for {
		l, err := buf.ReadBytes('\n')
		if err != nil {
			t.Error(err.Error())
			return 0
		}
		line := string(l)
		line = strings.Trim(line, " \t\n\r")
		if strings.HasPrefix(line, "LLVM version") {
			line = strings.Trim(line[12:], " \t")
			dot := strings.IndexByte(line, '.')
			if dot < 1 {
				t.Errorf("version=%s, dot=%d", line, dot)
				return 0
			}
			version, err := strconv.Atoi(line[:dot])
			if err != nil {
				t.Error(err.Error())
				return 0
			}
			return version
		}
	}
}
