package main

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

func TestParse(t *testing.T) {
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
		if strings.HasPrefix(name, "parse_success_") {
			testParseSuccess(t, filepath.Join("testdata", name))
		} else if strings.HasPrefix(name, "parse_fail_") {
			testParseFail(t, filepath.Join("testdata", name))
		}
	}
}

func testParseSuccess(t *testing.T, filename string) {
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
	prog, err := Parse(NewTokenizer(bytes.NewBuffer(contents), ""))
	if err != nil {
		t.Errorf("Parse %s: %s", filename, err.Error())
		return
	}
	routs := []string{}
	for k := range prog {
		routs = append(routs, k)
	}
	sort.Strings(routs)

	var b strings.Builder
	for _, k := range routs {
		prog[k].Unparse(&b)
	}

	src := b.String()
	if src != string(contents) {
		t.Errorf("Unparse %s:\n%s", filename, src)
	}
}

func testParseFail(t *testing.T, filename string) {
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
	_, err = Parse(NewTokenizer(bytes.NewBuffer(contents), ""))
	if err == nil {
		t.Errorf("Parse %s", filename)
		return
	}
	for i, b := range contents {
		if b == '\n' {
			msg := string(contents[2:i])
			if msg != err.Error() {
				t.Errorf("Parse %s: %s", filename, err.Error())
			}
			break
		}
	}
}
