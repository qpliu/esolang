package main

import (
	"io"
	"math/bits"
)

type ArrayValue struct {
	Dimensions []int
	Values     []uint32
}

type Var struct {
	Ignored bool
	Value   uint32
	Stash   []uint32
}

type ArrayVar struct {
	Ignored bool
	Value   ArrayValue
	Stash   []ArrayValue
}

type Expr interface {
	Eval(state *State) (uint32, bool, *Error) // result, is 16 bit, error
	MustBe16() bool
	MustBe32() bool
	ConstValue() (uint32, bool, bool) // result, is 16 bit, is const
}

type LValue interface {
	Gets(state *State, val uint32, is16 bool)
}

type Stashable interface {
	Stash(state *State)
	Retrieve(state *State) *Error
	Ignore(state *State)
	Remember(state *State)
}

type Dimensionable interface {
	Dimension(state *State, dimensions []int)
}

type ReadOutable interface {
	ReadOut(state *State, w io.Writer)
}

type WriteInable interface {
	WriteIn(state *State, r *IntercalReader) error
}

type Array16 uint16

func (v Array16) Stash(state *State) {
	//...
}

func (v Array16) Retrieve(state *State) *Error {
	//...
	return nil
}

func (v Array16) Ignore(state *State) {
	//...
}

func (v Array16) Remember(state *State) {
	//...
}

func (a Array16) Dimension(state *State, dimensions []int) {
	//...
}

func (a Array16) WriteIn(state *State, r *IntercalReader) error {
	//...
	return nil
}

type Array32 uint16

func (v Array32) Stash(state *State) {
	//...
}

func (v Array32) Retrieve(state *State) *Error {
	//...
	return nil
}

func (v Array32) Ignore(state *State) {
	//...
}

func (v Array32) Remember(state *State) {
	//...
}

func (a Array32) Dimension(state *State, dimensions []int) {
	//...
}

func (a Array32) WriteIn(state *State, r *IntercalReader) error {
	//...
	return nil
}

type Var16 uint16

func (v Var16) Eval(state *State) (uint32, bool, *Error) {
	//...
	return 0, true, Err774
}

func (v Var16) MustBe16() bool {
	return true
}

func (v Var16) MustBe32() bool {
	return false
}

func (v Var16) ConstValue() (uint32, bool, bool) {
	return 0, true, false
}

func (v Var16) Gets(state *State, val uint32, is16 bool) {
	//...
}

func (v Var16) Stash(state *State) {
	//...
}

func (v Var16) Retrieve(state *State) *Error {
	//...
	return nil
}

func (v Var16) Ignore(state *State) {
	//...
}

func (v Var16) Remember(state *State) {
	//...
}

func (v Var16) ReadOut(state *State, w io.Writer) {
	//...
}

func (v Var16) WriteIn(state *State, r *IntercalReader) error {
	//...
	return nil
}

type Var32 uint16

func (v Var32) Eval(state *State) (uint32, bool, *Error) {
	//...
	return 0, false, Err774
}

func (v Var32) MustBe16() bool {
	return false
}

func (v Var32) MustBe32() bool {
	return true
}

func (v Var32) ConstValue() (uint32, bool, bool) {
	return 0, false, false
}

func (v Var32) Gets(state *State, val uint32, is16 bool) {
	//...
}

func (v Var32) Stash(state *State) {
	//...
}

func (v Var32) Retrieve(state *State) *Error {
	//...
	return nil
}

func (v Var32) Ignore(state *State) {
	//...
}

func (v Var32) Remember(state *State) {
	//...
}

func (v Var32) ReadOut(state *State, w io.Writer) {
	//...
}

func (v Var32) WriteIn(state *State, r *IntercalReader) error {
	//...
	return nil
}

type ArrayElement struct {
	Array interface{}
	// Array is Array16 or Array32

	Index []Expr
}

func (e ArrayElement) Eval(state *State) (uint32, bool, *Error) {
	//...
	return 0, e.MustBe16(), Err774
}

func (e ArrayElement) MustBe16() bool {
	switch e.Array.(type) {
	case Array16:
		return true
	case Array32:
		return false
	default:
		panic("ArrayElement")
	}
}

func (e ArrayElement) MustBe32() bool {
	switch e.Array.(type) {
	case Array16:
		return false
	case Array32:
		return true
	default:
		panic("ArrayElement")
	}
}

func (e ArrayElement) ConstValue() (uint32, bool, bool) {
	return 0, e.MustBe16(), false
}

func (e ArrayElement) Gets(state *State, val uint32, is16 bool) {
	//...
}

func (e ArrayElement) ReadOut(state *State, w io.Writer) {
	//...
}

func (e ArrayElement) WriteIn(state *State, r *IntercalReader) error {
	//...
	return nil
}

type ExprConst uint16

func (e ExprConst) Eval(state *State) (uint32, bool, *Error) {
	return uint32(e), true, nil
}

func (e ExprConst) MustBe16() bool {
	return true
}

func (e ExprConst) MustBe32() bool {
	return false
}

func (e ExprConst) ConstValue() (uint32, bool, bool) {
	return uint32(e), true, true
}

func (e ExprConst) ReadOut(state *State, w io.Writer) {
	Output(w, uint32(e))
}

type ExprMingle [2]Expr

func (e ExprMingle) Eval(state *State) (uint32, bool, *Error) {
	l, _, err := e[0].Eval(state)
	if err != nil {
		return 0, false, err
	}
	r, _, err := e[1].Eval(state)
	if err != nil {
		return 0, false, err
	}
	return OpMingle(l, r), false, nil
}

func (e ExprMingle) MustBe16() bool {
	return false
}

func (e ExprMingle) MustBe32() bool {
	return true
}

func (e ExprMingle) ConstValue() (uint32, bool, bool) {
	l, _, lconst := e[0].ConstValue()
	if !lconst {
		return 0, false, false
	}
	r, _, rconst := e[1].ConstValue()
	if !rconst {
		return 0, false, false
	}
	return OpMingle(l, r), false, true
}

type ExprSelect [2]Expr

func (e ExprSelect) Eval(state *State) (uint32, bool, *Error) {
	l, _, err := e[0].Eval(state)
	if err != nil {
		return 0, false, err
	}
	r, _, err := e[1].Eval(state)
	if err != nil {
		return 0, false, err
	}
	return OpSelect(l, r), bits.OnesCount32(r) <= 16, nil
}

func (e ExprSelect) MustBe16() bool {
	r, _, rconst := e[1].ConstValue()
	if !rconst {
		return false
	}
	return bits.OnesCount32(r) <= 16
}

func (e ExprSelect) MustBe32() bool {
	r, _, rconst := e[1].ConstValue()
	if !rconst {
		return false
	}
	return bits.OnesCount32(r) > 16
}

func (e ExprSelect) ConstValue() (uint32, bool, bool) {
	l, _, lconst := e[0].ConstValue()
	if !lconst {
		return 0, false, false
	}
	r, _, rconst := e[1].ConstValue()
	if !rconst {
		return 0, false, false
	}
	return OpSelect(l, r), bits.OnesCount32(r) <= 16, true
}

type ExprAnd [1]Expr

func (e ExprAnd) Eval(state *State) (uint32, bool, *Error) {
	v, is16, err := e[0].Eval(state)
	if err != nil {
		return 0, is16, err
	}
	if is16 {
		return OpAnd16(v), true, nil
	} else {
		return OpAnd32(v), false, nil
	}
}

func (e ExprAnd) MustBe16() bool {
	return e[0].MustBe16()
}

func (e ExprAnd) MustBe32() bool {
	return e[0].MustBe32()
}

func (e ExprAnd) ConstValue() (uint32, bool, bool) {
	v, is16, vconst := e[0].ConstValue()
	if !vconst {
		return 0, is16, false
	}
	if is16 {
		return OpAnd16(v), true, true
	} else {
		return OpAnd32(v), false, true
	}
}

type ExprOr [1]Expr

func (e ExprOr) Eval(state *State) (uint32, bool, *Error) {
	v, is16, err := e[0].Eval(state)
	if err != nil {
		return 0, is16, err
	}
	if is16 {
		return OpOr16(v), true, nil
	} else {
		return OpOr32(v), false, nil
	}
}

func (e ExprOr) MustBe16() bool {
	return e[0].MustBe16()
}

func (e ExprOr) MustBe32() bool {
	return e[0].MustBe32()
}

func (e ExprOr) ConstValue() (uint32, bool, bool) {
	v, is16, vconst := e[0].ConstValue()
	if !vconst {
		return 0, is16, false
	}
	if is16 {
		return OpOr16(v), true, true
	} else {
		return OpOr32(v), false, true
	}
}

type ExprXor [1]Expr

func (e ExprXor) Eval(state *State) (uint32, bool, *Error) {
	v, is16, err := e[0].Eval(state)
	if err != nil {
		return 0, is16, err
	}
	if is16 {
		return OpXor16(v), true, nil
	} else {
		return OpXor32(v), false, nil
	}
}

func (e ExprXor) MustBe16() bool {
	return e[0].MustBe16()
}

func (e ExprXor) MustBe32() bool {
	return e[0].MustBe32()
}

func (e ExprXor) ConstValue() (uint32, bool, bool) {
	v, is16, vconst := e[0].ConstValue()
	if !vconst {
		return 0, is16, false
	}
	if is16 {
		return OpXor16(v), true, true
	} else {
		return OpXor32(v), false, true
	}
}
