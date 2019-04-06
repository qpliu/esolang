package main

import (
	"io"
)

type LibFunc interface {
	ArgCount() int
	ArgPushed(index int) bool
	ResultAliasesArg(index int) bool
	Call(callToken Token, r io.Reader, w io.Writer, args []*Value) (*Value, bool, error)
}

func GetLibFunc(lib Token, nParams int) (LibFunc, error) {
	if lib.Value == "read" && nParams == 0 {
		return &libFuncRead{
			bitIndex:   0,
			byteBuffer: []byte{0},
		}, nil
	} else if lib.Value == "write" && nParams == 1 {
		return &libFuncWrite{
			bitIndex:   1,
			byteBuffer: []byte{0},
		}, nil
	} else if lib.Value == "automated testing function" {
		return libFuncAutomatedTestingFunction(nParams), nil
	} else if lib.Value == "automated testing value inspector" && nParams == 1 {
		return libFuncAutomatedTestingValueInspector{}, nil
	}
	return nil, lib.Errorf("Undefined %d-argument library function %s", nParams, lib.Value)
}

type libFuncRead struct {
	bitIndex   byte
	byteBuffer []byte
}

func (f *libFuncRead) ArgCount() int {
	return 0
}

func (f *libFuncRead) ArgPushed(index int) bool {
	return false
}

func (f *libFuncRead) ResultAliasesArg(index int) bool {
	return false
}

func (f *libFuncRead) Call(callToken Token, r io.Reader, w io.Writer, args []*Value) (*Value, bool, error) {
	f.bitIndex <<= 1
	if f.bitIndex == 0 {
		n, err := r.Read(f.byteBuffer)
		if n == 0 {
			if err == io.EOF {
				return &Value{}, false, nil
			} else {
				return nil, false, err
			}
		}
		f.bitIndex = 1
	}
	zero := &Value{}
	zero.Push(&Value{})
	if f.byteBuffer[0]&f.bitIndex == 0 {
		return zero, false, nil
	}
	one := &Value{}
	one.Push(zero)
	return one, false, nil
}

type libFuncWrite struct {
	bitIndex   byte
	byteBuffer []byte
}

func (f *libFuncWrite) ArgCount() int {
	return 1
}

func (f *libFuncWrite) ArgPushed(index int) bool {
	return false
}

func (f *libFuncWrite) ResultAliasesArg(index int) bool {
	return true
}

func (f *libFuncWrite) Call(callToken Token, r io.Reader, w io.Writer, args []*Value) (*Value, bool, error) {
	if len(args) != 1 || args[0] == nil {
		panic("lib:write")
	}
	if args[0].Size() > 1 {
		f.byteBuffer[0] |= f.bitIndex
	}
	f.bitIndex <<= 1
	if f.bitIndex == 0 {
		_, err := w.Write(f.byteBuffer)
		if err != nil {
			return nil, false, err
		}
		f.bitIndex = 1
		f.byteBuffer[0] = 0
	}
	return args[0], false, nil
}

type libFuncAutomatedTestingFunction int

func (f libFuncAutomatedTestingFunction) ArgCount() int {
	return int(f)
}

func (f libFuncAutomatedTestingFunction) ArgPushed(index int) bool {
	return false
}

func (f libFuncAutomatedTestingFunction) ResultAliasesArg(index int) bool {
	return index == 0 && f > 0
}

func (f libFuncAutomatedTestingFunction) Call(callToken Token, r io.Reader, w io.Writer, args []*Value) (*Value, bool, error) {
	call := []int{}
	for _, arg := range args {
		call = append(call, arg.Size())
	}
	automatedTestingFunctionCalls = append(automatedTestingFunctionCalls, call)
	if len(args) == 0 {
		return &Value{}, false, nil
	}
	return args[0], false, nil
}

var automatedTestingFunctionCalls [][]int

func AutomatedTestingFunctionCalls() [][]int {
	calls := automatedTestingFunctionCalls
	automatedTestingFunctionCalls = [][]int{}
	return calls
}

type libFuncAutomatedTestingValueInspector struct{}

func (libFuncAutomatedTestingValueInspector) ArgCount() int {
	return 1
}

func (libFuncAutomatedTestingValueInspector) ArgPushed(index int) bool {
	return false
}

func (libFuncAutomatedTestingValueInspector) ResultAliasesArg(index int) bool {
	return true
}

func (libFuncAutomatedTestingValueInspector) Call(callToken Token, r io.Reader, w io.Writer, args []*Value) (*Value, bool, error) {
	if len(args) != 1 || args[0] == nil {
		panic("lib:automated testing value inspector")
	}
	var writeVal func(*Value) error
	writeVal = func(val *Value) error {
		if _, err := w.Write([]byte{'['}); err != nil {
			return err
		}
		for _, v := range val.stack {
			if err := writeVal(v); err != nil {
				return err
			}
		}
		if _, err := w.Write([]byte{']'}); err != nil {
			return err
		}
		return nil
	}
	if err := writeVal(args[0]); err != nil {
		return args[0], false, err
	}
	if _, err := w.Write([]byte{'\n'}); err != nil {
		return args[0], false, err
	}
	return args[0], false, nil
}
