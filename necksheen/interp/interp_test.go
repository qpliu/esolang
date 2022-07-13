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
	stmt, err := Parse(NewTokenizer(bytes.NewBuffer(contents), filename))
	if err != nil {
		t.Errorf("%s", err.Error())
		return
	}

	input := ""
	expectedOutput := ""
	if len(contents) > 2 && contents[0] == '=' && contents[1] == '=' {
	loop:
		for i := 2; i < len(contents); i++ {
			switch contents[i] {
			case '\n':
				input = string(contents[2:i])
				break loop
			case ',':
				input = string(contents[2:i])
				i++
				for j := i; j < len(contents); j++ {
					switch contents[j] {
					case '\n':
						expectedOutput = string(contents[i:j])
						break loop
					}
				}
				break loop
			}
		}
	}

	ioQueue := NewTestingQueue(input)
	Interp(stmt, ioQueue)
	if expectedOutput != ioQueue.Output() {
		t.Errorf("%s: output:%s, expected:%s", filename, ioQueue.Output(), expectedOutput)
	}
}
