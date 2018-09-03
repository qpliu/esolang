package intercal

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

func testCodeGen(t *testing.T, srcFile string) {
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

	tmpFile := filepath.Join(os.TempDir(), fmt.Sprintf("codegen_test.%s.%d", filepath.Base(srcFile), os.Getpid()))

	if err := func() error {
		f, err := os.Create(tmpFile + ".ll")
		if err != nil {
			return err
		}
		defer f.Close()
		return CodeGen(statements, f)
	}(); err != nil {
		t.Errorf("codegen %s: %s", srcFile, err.Error())
		return
	}

	if testing.Short() {
		os.Remove(tmpFile)
		return
	}

	cmd := fmt.Sprintf("cat runtime.ll %s.ll | llc > %s.s", tmpFile, tmpFile)
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
			testCodeGen(t, name)
		}
	}
}
