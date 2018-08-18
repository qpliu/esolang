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
	Gets(state *State, value uint32, is16 bool) *Error
	Ignored(state *State) bool
}

type Stashable interface {
	Stash(state *State)
	Retrieve(state *State) *Error
	Ignore(state *State)
	Remember(state *State)
	Ignored(state *State) bool
}

type Dimensionable interface {
	Dimension(state *State, dimensions []int) *Error
}

type ReadOutable interface {
	ReadOut(state *State, w io.Writer)
}

type WriteInable interface {
	WriteIn(state *State, r *IntercalReader) *Error
}

type Array16 uint16

func (v Array16) Stash(state *State) {
	val := state.Array16(v)
	if val.Ignored {
		return
	}
	val.Stash = append(val.Stash, ArrayValue{
		Dimensions: val.Value.Dimensions,
		Values:     append([]uint32{}, val.Value.Values...),
	})
}

func (v Array16) Retrieve(state *State) *Error {
	val := state.Array16(v)
	if val.Ignored {
		return nil
	}
	if len(val.Stash) == 0 {
		return Err436
	}
	val.Value = val.Stash[len(val.Stash)-1]
	val.Stash = val.Stash[:len(val.Stash)-1]
	return nil
}

func (v Array16) Ignore(state *State) {
	state.Array16(v).Ignored = true
}

func (v Array16) Remember(state *State) {
	state.Array16(v).Ignored = false
}

func (v Array16) Ignored(state *State) bool {
	return state.Array16(v).Ignored
}

func (v Array16) Dimension(state *State, dimensions []int) *Error {
	val := state.Array16(v)
	if val.Ignored {
		return nil
	}
	if len(dimensions) == 0 {
		return Err436
	}
	size := 1
	for _, dim := range dimensions {
		size *= dim
	}
	if size == 0 {
		return Err436
	}
	val.Value.Dimensions = dimensions
	val.Value.Values = make([]uint32, size)
	return nil
}

func (v Array16) WriteIn(state *State, r *IntercalReader) *Error {
	val := state.Array16(v)
	if val.Ignored {
		return nil
	}
	for i := range val.Value.Values {
		n, err := r.Input16()
		if err != nil {
			if e, ok := err.(*Error); ok {
				return e
			} else {
				return Err562
			}
		}
		val.Value.Values[i] = uint32(n)
	}
	return nil
}

type Array32 uint16

func (v Array32) Stash(state *State) {
	val := state.Array32(v)
	if val.Ignored {
		return
	}
	val.Stash = append(val.Stash, ArrayValue{
		Dimensions: val.Value.Dimensions,
		Values:     append([]uint32{}, val.Value.Values...),
	})
}

func (v Array32) Retrieve(state *State) *Error {
	val := state.Array32(v)
	if val.Ignored {
		return nil
	}
	if len(val.Stash) == 0 {
		return Err436
	}
	val.Value = val.Stash[len(val.Stash)-1]
	val.Stash = val.Stash[:len(val.Stash)-1]
	return nil
}

func (v Array32) Ignore(state *State) {
	state.Array32(v).Ignored = true
}

func (v Array32) Remember(state *State) {
	state.Array32(v).Ignored = false
}

func (v Array32) Ignored(state *State) bool {
	return state.Array32(v).Ignored
}

func (v Array32) Dimension(state *State, dimensions []int) *Error {
	val := state.Array32(v)
	if val.Ignored {
		return nil
	}
	if len(dimensions) == 0 {
		return Err436
	}
	size := 1
	for _, dim := range dimensions {
		size *= dim
	}
	if size == 0 {
		return Err436
	}
	val.Value.Dimensions = dimensions
	val.Value.Values = make([]uint32, size)
	return nil
}

func (v Array32) WriteIn(state *State, r *IntercalReader) *Error {
	val := state.Array32(v)
	if val.Ignored {
		return nil
	}
	for i := range val.Value.Values {
		n, err := r.Input32()
		if err != nil {
			if e, ok := err.(*Error); ok {
				return e
			} else {
				return Err562
			}
		}
		val.Value.Values[i] = n
	}
	return nil
}

type Var16 uint16

func (v Var16) Eval(state *State) (uint32, bool, *Error) {
	return state.Var16(v).Value, true, nil
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

func (v Var16) Gets(state *State, value uint32, is16 bool) *Error {
	val := state.Var16(v)
	if val.Ignored {
		return nil
	}
	if !is16 && value >= 65535 {
		return Err275
	}
	val.Value = value & 65535
	return nil
}

func (v Var16) Stash(state *State) {
	val := state.Var16(v)
	if val.Ignored {
		return
	}
	val.Stash = append(val.Stash, val.Value)
}

func (v Var16) Retrieve(state *State) *Error {
	val := state.Var16(v)
	if val.Ignored {
		return nil
	}
	if len(val.Stash) == 0 {
		return Err436
	}
	val.Value = val.Stash[len(val.Stash)-1]
	val.Stash = val.Stash[:len(val.Stash)-1]
	return nil
}

func (v Var16) Ignore(state *State) {
	state.Var16(v).Ignored = true
}

func (v Var16) Remember(state *State) {
	state.Var16(v).Ignored = false
}

func (v Var16) Ignored(state *State) bool {
	return state.Var16(v).Ignored
}

func (v Var16) ReadOut(state *State, w io.Writer) {
	Output(w, state.Var16(v).Value)
}

func (v Var16) WriteIn(state *State, r *IntercalReader) *Error {
	val := state.Var16(v)
	if val.Ignored {
		return nil
	}
	n, err := r.Input16()
	if err != nil {
		if e, ok := err.(*Error); ok {
			return e
		} else {
			return Err562
		}
	}
	val.Value = uint32(n)
	return nil
}

type Var32 uint16

func (v Var32) Eval(state *State) (uint32, bool, *Error) {
	return state.Var32(v).Value, true, nil
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

func (v Var32) Gets(state *State, value uint32, is16 bool) *Error {
	val := state.Var32(v)
	if val.Ignored {
		return nil
	}
	val.Value = value
	return nil
}

func (v Var32) Stash(state *State) {
	val := state.Var32(v)
	if val.Ignored {
		return
	}
	val.Stash = append(val.Stash, val.Value)
}

func (v Var32) Retrieve(state *State) *Error {
	val := state.Var32(v)
	if val.Ignored {
		return nil
	}
	if len(val.Stash) == 0 {
		return Err436
	}
	val.Value = val.Stash[len(val.Stash)-1]
	val.Stash = val.Stash[:len(val.Stash)-1]
	return nil
}

func (v Var32) Ignore(state *State) {
	state.Var32(v).Ignored = true
}

func (v Var32) Remember(state *State) {
	state.Var32(v).Ignored = false
}

func (v Var32) Ignored(state *State) bool {
	return state.Var32(v).Ignored
}

func (v Var32) ReadOut(state *State, w io.Writer) {
	Output(w, state.Var32(v).Value)
}

func (v Var32) WriteIn(state *State, r *IntercalReader) *Error {
	val := state.Var32(v)
	if val.Ignored {
		return nil
	}
	n, err := r.Input32()
	if err != nil {
		if e, ok := err.(*Error); ok {
			return e
		} else {
			return Err562
		}
	}
	val.Value = n
	return nil
}

type ArrayElement struct {
	Array Stashable
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

func (e ArrayElement) Gets(state *State, value uint32, is16 bool) *Error {
	//...
	return nil
}

func (e ArrayElement) Ignored(state *State) bool {
	return e.Array.Ignored(state)
}

func (e ArrayElement) ReadOut(state *State, w io.Writer) {
	//...
}

func (e ArrayElement) WriteIn(state *State, r *IntercalReader) *Error {
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
