package main

import (
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

type Array16 uint16
type Array32 uint16

type Expr interface {
	Eval(state *State) (uint32, error)
	MustBe16() bool
	MustBe32() bool
	ConstValue() (uint32, bool)
}

type Var16 uint16

func (v Var16) Eval(state *State) (uint32, error) {
	//...
	return 0, Err774
}

func (v Var16) MustBe16() bool {
	return true
}

func (v Var16) MustBe32() bool {
	return false
}

func (v Var16) ConstValue() (uint32, bool) {
	return 0, false
}

type Var32 uint16

func (v Var32) Eval(state *State) (uint32, error) {
	//...
	return 0, Err774
}

func (v Var32) MustBe16() bool {
	return false
}

func (v Var32) MustBe32() bool {
	return true
}

func (v Var32) ConstValue() (uint32, bool) {
	return 0, false
}

type ArrayElement struct {
	Array interface{}
	// Array is Array16 or Array32

	Index []Expr
}

func (e ArrayElement) Eval(state *State) (uint32, error) {
	//...
	return 0, Err774
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

func (e ArrayElement) ConstValue() (uint32, bool) {
	return 0, false
}

type ExprConst uint16

func (e ExprConst) Eval(state *State) (uint32, error) {
	return uint32(e), nil
}

func (e ExprConst) MustBe16() bool {
	return true
}

func (e ExprConst) MustBe32() bool {
	return false
}

func (e ExprConst) ConstValue() (uint32, bool) {
	return uint32(e), true
}

type ExprMingle [2]Expr

func (e ExprMingle) Eval(state *State) (uint32, error) {
	l, err := e[0].Eval(state)
	if err != nil {
		return 0, err
	}
	r, err := e[1].Eval(state)
	if err != nil {
		return 0, err
	}
	return OpMingle(l, r), nil
}

func (e ExprMingle) MustBe16() bool {
	return false
}

func (e ExprMingle) MustBe32() bool {
	return true
}

func (e ExprMingle) ConstValue() (uint32, bool) {
	l, lconst := e[0].ConstValue()
	if !lconst {
		return 0, false
	}
	r, rconst := e[1].ConstValue()
	if !rconst {
		return 0, false
	}
	return OpMingle(l, r), true
}

type ExprSelect [2]Expr

func (e ExprSelect) Eval(state *State) (uint32, error) {
	l, err := e[0].Eval(state)
	if err != nil {
		return 0, err
	}
	r, err := e[1].Eval(state)
	if err != nil {
		return 0, err
	}
	return OpSelect(l, r), nil
}

func (e ExprSelect) MustBe16() bool {
	rval, rconst := e[1].ConstValue()
	if !rconst {
		return false
	}
	return bits.OnesCount32(rval) <= 16
}

func (e ExprSelect) MustBe32() bool {
	rval, rconst := e[1].ConstValue()
	if !rconst {
		return false
	}
	return bits.OnesCount32(rval) > 16
}

func (e ExprSelect) ConstValue() (uint32, bool) {
	l, lconst := e[0].ConstValue()
	if !lconst {
		return 0, false
	}
	r, rconst := e[1].ConstValue()
	if !rconst {
		return 0, false
	}
	return OpSelect(l, r), true
}

type ExprAnd [1]Expr

func (e ExprAnd) Eval(state *State) (uint32, error) {
	v, err := e[0].Eval(state)
	if err != nil {
		return 0, err
	}
	if e[0].MustBe16() {
		return OpAnd16(v), nil
	} else if e[0].MustBe32() {
		return OpAnd32(v), nil
	} else if v < 65536 {
		return OpAnd16(v), nil
	} else {
		return OpAnd32(v), nil
	}
}

func (e ExprAnd) MustBe16() bool {
	return e[0].MustBe16()
}

func (e ExprAnd) MustBe32() bool {
	return e[0].MustBe32()
}

func (e ExprAnd) ConstValue() (uint32, bool) {
	v, vconst := e[0].ConstValue()
	if !vconst {
		return 0, false
	}
	if e[0].MustBe16() {
		return OpAnd16(v), true
	} else if e[0].MustBe32() {
		return OpAnd32(v), true
	} else if v < 65536 {
		return OpAnd16(v), true
	} else {
		return OpAnd32(v), true
	}
}

type ExprOr [1]Expr

func (e ExprOr) Eval(state *State) (uint32, error) {
	v, err := e[0].Eval(state)
	if err != nil {
		return 0, err
	}
	if e[0].MustBe16() {
		return OpOr16(v), nil
	} else if e[0].MustBe32() {
		return OpOr32(v), nil
	} else if v < 65536 {
		return OpOr16(v), nil
	} else {
		return OpOr32(v), nil
	}
}

func (e ExprOr) MustBe16() bool {
	return e[0].MustBe16()
}

func (e ExprOr) MustBe32() bool {
	return e[0].MustBe32()
}

func (e ExprOr) ConstValue() (uint32, bool) {
	v, vconst := e[0].ConstValue()
	if !vconst {
		return 0, false
	}
	if e[0].MustBe16() {
		return OpOr16(v), true
	} else if e[0].MustBe32() {
		return OpOr32(v), true
	} else if v < 65536 {
		return OpOr16(v), true
	} else {
		return OpOr32(v), true
	}
}

type ExprXor [1]Expr

func (e ExprXor) Eval(state *State) (uint32, error) {
	v, err := e[0].Eval(state)
	if err != nil {
		return 0, err
	}
	if e[0].MustBe16() {
		return OpXor16(v), nil
	} else if e[0].MustBe32() {
		return OpXor32(v), nil
	} else if v < 65536 {
		return OpXor16(v), nil
	} else {
		return OpXor32(v), nil
	}
}

func (e ExprXor) MustBe16() bool {
	return e[0].MustBe16()
}

func (e ExprXor) MustBe32() bool {
	return e[0].MustBe32()
}

func (e ExprXor) ConstValue() (uint32, bool) {
	v, vconst := e[0].ConstValue()
	if !vconst {
		return 0, false
	}
	if e[0].MustBe16() {
		return OpXor16(v), true
	} else if e[0].MustBe32() {
		return OpXor32(v), true
	} else if v < 65536 {
		return OpXor16(v), true
	} else {
		return OpXor32(v), true
	}
}
