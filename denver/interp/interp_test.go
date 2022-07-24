package main

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestInterp(t *testing.T) {
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
		if strings.HasPrefix(name, "interp_") {
			testInterp(t, filepath.Join("testdata", name))
		}
	}
}

func testInterp(t *testing.T, filename string) {
	file, err := os.Open(filename)
	if err != nil {
		t.Errorf("Open %s: %s", filename, err.Error())
		return
	}
	defer file.Close()
	contents, err := io.ReadAll(file)
	if err != nil {
		t.Errorf("ReadAll %s: %s", filename, err.Error())
		return
	}
	prog, err := Parse(NewTokenizer(bytes.NewBuffer(contents), filename))
	if err != nil {
		t.Errorf("%s", err.Error())
		return
	}

	input := []bool{}
	expectedOutput := []bool{}
	if len(contents) > 2 && contents[0] == '=' && contents[1] == '=' {
		gotEnd := false
	loop:
		for i := 2; i < len(contents); i++ {
			switch contents[i] {
			case '\n':
				break loop
			case ',':
				gotEnd = true
			case '0':
				if gotEnd {
					expectedOutput = append(expectedOutput, false)
				} else {
					input = append(input, false)
				}
			case '1':
				if gotEnd {
					expectedOutput = append(expectedOutput, true)
				} else {
					input = append(input, true)
				}
			}
		}
	}

	out := newTestOutput()
	sys := SystemThread(&testInput{bits: input}, out)
	Interp(prog, prog["main"], NewThread(), []*Thread{sys})
	sys.SignalTerminating()
	sys.WaitTerminated()

	out.Check(t, filename, expectedOutput...)
}
