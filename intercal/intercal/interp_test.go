package intercal

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func testSrcFile(t *testing.T, srcFilename string) {
	tokenizer, err := NewFileTokenizer([]string{filepath.Join("testdata", srcFilename)})
	if err != nil {
		t.Errorf("parse %s: %s", srcFilename, err.Error())
		return
	}
	statements, err := Parse(tokenizer)
	if err != nil {
		t.Errorf("parse %s: %s", srcFilename, err.Error())
		return
	}
	var input *IntercalReader
	inputFile, err := os.Open(filepath.Join("testdata", srcFilename+".in"))
	if err == nil {
		defer inputFile.Close()
		input = NewIntercalReader(inputFile)
	}

	output := &bytes.Buffer{}
	state := NewState(statements)
	state.Random.Seed(0)
	if err := state.Run(input, output); err != nil && !strings.HasSuffix(srcFilename, fmt.Sprintf("E%03d.i", err.Code())) {
		t.Errorf("%s:%s", srcFilename, err.Error())
		return
	}
	actual := output.String()

	expected := func() string {
		data, err := ioutil.ReadFile(filepath.Join("testdata", srcFilename+".out"))
		if err != nil {
			return ""
		} else {
			return string(data)
		}
	}()
	if expected != actual {
		t.Errorf("%s output expected=%s\nactual=%s", srcFilename, expected, actual)
	}
}

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
		if filepath.Ext(name) == ".i" || filepath.Ext(name) == ".I" {
			testSrcFile(t, name)
		}
	}
}
