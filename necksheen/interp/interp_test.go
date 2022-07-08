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
	stmt, err := Parse(NewTokenizer(bytes.NewBuffer(contents)))
	if err != nil {
		t.Errorf("Parse %s, %s", filename, err.Error())
		return
	}
	leftIn := []bool{}
	leftOut := []bool{}
	rightIn := []bool{}
	rightOut := []bool{}
	ioIn := []bool{}
	ioOut := []bool{}
	lineStart := 0
	fill := func(in *[]bool, out *[]bool) {
		b := in
		for i := lineStart; contents[i] != '\n'; i++ {
			switch contents[i] {
			case '0':
				*b = append(*b, false)
			case '1':
				*b = append(*b, true)
			case ',':
				b = out
			}
		}
	}
	for i, b := range contents {
		if b != '\n' {
			continue
		}
		if i-lineStart > 7 && string(contents[lineStart:lineStart+7]) == "==left:" {
			fill(&leftIn, &leftOut)
		} else if i-lineStart > 8 && string(contents[lineStart:lineStart+8]) == "==right:" {
			fill(&rightIn, &rightOut)
		} else if i-lineStart > 5 && string(contents[lineStart:lineStart+5]) == "==io:" {
			fill(&ioIn, &ioOut)
		} else {
			break
		}
		lineStart = i + 1
	}
	leftIn = append(leftIn, false)   // needed for select
	rightIn = append(rightIn, false) // needed for select
	ioIn = append(ioIn, false)       // needed for select

	left := [2]chan bool{make(chan bool), make(chan bool)}
	right := [2]chan bool{make(chan bool), make(chan bool)}
	io := [2]chan bool{make(chan bool), make(chan bool)}
	done := make(chan bool)
	go func() {
		Interpret(stmt, map[string]bool{"0": false}, map[string][2]chan bool{
			"left":  left,
			"right": right,
			"io":    io,
		})
		done <- true
	}()
loop:
	for {
		select {
		case left[0] <- leftIn[0]:
			if len(leftIn) > 1 {
				leftIn = leftIn[1:]
			} else {
				t.Errorf("%s: receive left", filename)
			}
		case right[0] <- rightIn[0]:
			if len(rightIn) > 1 {
				rightIn = rightIn[1:]
			} else {
				t.Errorf("%s: receive right", filename)
			}
		case io[0] <- ioIn[0]:
			if len(ioIn) > 1 {
				ioIn = ioIn[1:]
			} else {
				t.Errorf("%s: receive io", filename)
			}
		case b := <-left[1]:
			if len(leftOut) > 0 && b == leftOut[0] {
				leftOut = leftOut[1:]
			} else {
				t.Errorf("%s: send left", filename)
			}
		case b := <-right[1]:
			if len(rightOut) > 0 && b == rightOut[0] {
				rightOut = rightOut[1:]
			} else {
				t.Errorf("%s: send right", filename)
			}
		case b := <-io[1]:
			if len(ioOut) > 0 && b == ioOut[0] {
				ioOut = ioOut[1:]
			} else {
				t.Errorf("%s: send io", filename)
			}
		case <-done:
			break loop
		}
	}
	if len(leftIn) != 1 || len(rightIn) != 1 || len(ioIn) != 1 {
		t.Errorf("%s: receive %d %d %d", filename, len(leftIn), len(rightIn), len(ioIn))
	}
	if len(leftOut) != 0 || len(rightOut) != 0 || len(ioOut) != 0 {
		t.Errorf("%s: send %d %d %d", filename, len(leftOut), len(rightOut), len(ioOut))
	}
}
