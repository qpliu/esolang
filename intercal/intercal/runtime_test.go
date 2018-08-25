package intercal

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

func testRuntime(t *testing.T, srcFile string) {
	tmpFile := filepath.Join(os.TempDir(), fmt.Sprintf("runtime_test.%s.%d", filepath.Base(srcFile), os.Getpid()))
	cmd := fmt.Sprintf("cat runtime.ll %s | llc > %s.s", srcFile, tmpFile)
	if err := exec.Command("sh", "-c", cmd).Run(); err != nil {
		t.Errorf("%s", cmd)
		return
	}
	cmd = fmt.Sprintf("cc -o %s %s.s", tmpFile, tmpFile)
	if err := exec.Command("sh", "-c", cmd).Run(); err != nil {
		t.Errorf("%s", cmd)
		return
	}
	for i := 1; ; i++ {
		outFile := fmt.Sprintf("%s.%03d.out", srcFile, i)
		if _, err := os.Stat(outFile); err != nil {
			break
		}
		inFile := fmt.Sprintf("%s.%03d.in", srcFile, i)
		if _, err := os.Stat(inFile); err != nil {
			cmd = fmt.Sprintf("%s 2>&1 | diff -q %s -", tmpFile, outFile)
		} else {
			cmd = fmt.Sprintf("%s < %s 2>&1 | diff -q %s -", tmpFile, inFile, outFile)
		}
		if err := exec.Command("sh", "-c", cmd).Run(); err != nil {
			t.Errorf("%s", cmd)
			continue
		}
	}
}

func TestRuntime(t *testing.T) {
	f, err := os.Open(filepath.Join("testdata", "runtime"))
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
		if filepath.Ext(name) == ".ll" {
			testRuntime(t, filepath.Join("testdata", "runtime", name))
		}
	}
}
