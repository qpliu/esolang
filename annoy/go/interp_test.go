package main

import (
	"bufio"
	"bytes"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

func testSrcFile(t *testing.T, filename string) {
	fn := filepath.Join("testdata", filename)
	expectedTestingCalls, expectedErrMsg, input, expectedOutput, err := getExpectedTestingCalls(fn)
	if err != nil {
		t.Errorf("%s: %s", fn, err.Error())
		return
	}
	stmts, err := parseSrcFile(fn)
	if err != nil {
		t.Errorf("%s: %s", fn, err.Error())
		return
	}

	outputBuffer := &bytes.Buffer{}
	err = Interp(bytes.NewReader([]byte(input)), outputBuffer, stmts)

	if err != nil {
		if expectedErrMsg == "" {
			t.Errorf("%s: Unexpected error=%s", filename, err.Error())
		} else if err.Error() != expectedErrMsg {
			t.Errorf("%s: Expected error=%s, got error=%s", filename, expectedErrMsg, err.Error())
		}
	} else if expectedErrMsg != "" {
		t.Errorf("%s: Did not get expected error=%s", filename, expectedErrMsg)
	}

	expectedOutput = strings.Trim(expectedOutput, " \t\r\n")
	output := strings.Trim(outputBuffer.String(), " \t\r\n")
	if expectedOutput != output {
		t.Errorf("%s: Expected output=%s, got output=%s", filename, expectedOutput, output)
	}

	testingCalls := AutomatedTestingFunctionCalls()
	if len(expectedTestingCalls) != len(testingCalls) {
		t.Errorf("%s: Expected %d testing call(s), got %d testing call(s)", filename, expectedTestingCalls, testingCalls)
		return
	}
	for i, call := range testingCalls {
		expectedCall := expectedTestingCalls[i]
		if len(expectedCall) != len(call) {
			t.Errorf("%s: Expected %d testing call(s), got %d testing call(s)", filename, expectedTestingCalls, testingCalls)
			return
		}
		for j := range call {
			if expectedCall[j] != call[j] {
				t.Errorf("%s: Expected %d testing call(s), got %d testing call(s)", filename, expectedTestingCalls, testingCalls)
				return
			}
		}
	}
}

func getExpectedTestingCalls(filename string) ([][]int, string, string, string, error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, "", "", "", err
	}
	defer f.Close()
	testingCalls := [][]int{}
	errMsg := ""
	input := ""
	output := ""
	r := bufio.NewReader(f)
	for {
		line, err := r.ReadString('\n')
		if err != nil && err != io.EOF {
			return testingCalls, errMsg, input, output, err
		}
		if strings.HasPrefix(line, "// CALL:") {
			call := []int{}
			for _, tok := range strings.Split(strings.Trim(line[8:], " \t\r\n"), " ") {
				if tok == "" {
					continue
				}
				i, err := strconv.Atoi(tok)
				if err != nil {
					return testingCalls, errMsg, input, output, err
				}
				call = append(call, i)
			}
			testingCalls = append(testingCalls, call)
		} else if strings.HasPrefix(line, "// ERR:") {
			errMsg = strings.Trim(line[7:], " \t\r\n")
		} else if strings.HasPrefix(line, "// INPUT:") {
			input += line[9:]
		} else if strings.HasPrefix(line, "// OUTPUT:") {
			output += line[10:]
		}
		if err == io.EOF {
			return testingCalls, errMsg, input, output, nil
		}
	}
}

func parseSrcFile(filename string) ([]Stmt, error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	return Parse(NewTokenizer(filename, f))
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
		if filepath.Ext(name) == ".annoy" {
			testSrcFile(t, name)
		}
	}
}
