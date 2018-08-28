package intercal

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
	ConstValue() (uint32, bool, bool)         // result, is 16 bit, has const value
}

type LValue interface {
	Gets(state *State, value uint32, is16 bool) *Error
	Ignoredable
}

type Ignoredable interface {
	Ignored(state *State) bool
}

type Stashable interface {
	Stash(state *State)
	Retrieve(state *State) *Error
	Ignore(state *State)
	Remember(state *State)
	Ignoredable
}

type Dimensionable interface {
	Dimension(state *State, dimensions []int) *Error
	Ignoredable
}

type ReadOutable interface {
	Expr
	ReadOut(state *State, w io.Writer) *Error
}

type WriteInable interface {
	WriteIn(state *State, r *IntercalReader) *Error
	Ignoredable
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
		return Err241
	}
	size := 1
	for _, dim := range dimensions {
		size *= dim
	}
	if size == 0 {
		return Err240
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
		return Err241
	}
	size := 1
	for _, dim := range dimensions {
		size *= dim
	}
	if size == 0 {
		return Err240
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

func (v Var16) ConstValue() (uint32, bool, bool) {
	return 0, true, false
}

func (v Var16) Gets(state *State, value uint32, is16 bool) *Error {
	val := state.Var16(v)
	if val.Ignored {
		return nil
	}
	if !is16 && value >= 65536 {
		return Err275
	}
	if !is16 && value == 65535 {
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

func (v Var16) ReadOut(state *State, w io.Writer) *Error {
	Output(w, state.Var16(v).Value)
	return nil
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
	return state.Var32(v).Value, false, nil
}

func (v Var32) ConstValue() (uint32, bool, bool) {
	return 0, true, false
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

func (v Var32) ReadOut(state *State, w io.Writer) *Error {
	Output(w, state.Var32(v).Value)
	return nil
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

func (v ArrayElement) arrayValue(state *State) *ArrayVar {
	if v.is16() {
		return state.Array16(v.Array.(Array16))
	} else {
		return state.Array32(v.Array.(Array32))
	}
}

func (v ArrayElement) arrayIndex(state *State, val *ArrayVar) (int, *Error) {
	if len(val.Value.Dimensions) == 0 || len(val.Value.Dimensions) != len(v.Index) {
		return 0, Err241
	}
	index := 0
	step := 1
	for i := range v.Index {
		sub, _, err := v.Index[i].Eval(state)
		if err != nil {
			return 0, err
		}
		if sub == 0 || int(sub) > val.Value.Dimensions[i] {
			return 0, Err241
		}
		index += step * int(sub-1)
		step *= val.Value.Dimensions[i]
	}
	return index, nil
}

func (v ArrayElement) Eval(state *State) (uint32, bool, *Error) {
	val := v.arrayValue(state)
	index, err := v.arrayIndex(state, val)
	if err != nil {
		return 0, v.is16(), err
	}
	return val.Value.Values[index], v.is16(), nil
}

func (v ArrayElement) ConstValue() (uint32, bool, bool) {
	return 0, v.is16(), false
}

func (v ArrayElement) is16() bool {
	switch v.Array.(type) {
	case Array16:
		return true
	case Array32:
		return false
	default:
		panic("ArrayElement")
	}
}

func (v ArrayElement) Gets(state *State, value uint32, is16 bool) *Error {
	val := v.arrayValue(state)
	if val.Ignored {
		return nil
	}
	if !is16 && v.is16() && value >= 65536 {
		return Err275
	}
	if !is16 && v.is16() && value == 65535 {
		return Err275
	}
	index, err := v.arrayIndex(state, val)
	if err != nil {
		return err
	}
	if v.is16() {
		value &= 65535
	}
	val.Value.Values[index] = value
	return nil
}

func (v ArrayElement) Ignored(state *State) bool {
	return v.Array.Ignored(state)
}

func (v ArrayElement) ReadOut(state *State, w io.Writer) *Error {
	value, _, err := v.Eval(state)
	if err != nil {
		return err
	}
	Output(w, value)
	return nil
}

func (v ArrayElement) WriteIn(state *State, r *IntercalReader) *Error {
	if v.Ignored(state) {
		return nil
	}
	val := v.arrayValue(state)
	if val.Ignored {
		return nil
	}
	index, err := v.arrayIndex(state, val)
	if err != nil {
		return err
	}
	var value uint32
	var errInput error
	if v.is16() {
		n, err := r.Input16()
		value, errInput = uint32(n), err
	} else {
		value, errInput = r.Input32()
	}
	if errInput != nil {
		if e, ok := errInput.(*Error); ok {
			return e
		} else {
			return Err562
		}
	}
	val.Value.Values[index] = value
	return nil
}

type ExprConst uint16

func (e ExprConst) Eval(state *State) (uint32, bool, *Error) {
	return uint32(e), true, nil
}

func (e ExprConst) ConstValue() (uint32, bool, bool) {
	return uint32(e), true, true
}

func (e ExprConst) ReadOut(state *State, w io.Writer) *Error {
	Output(w, uint32(e))
	return nil
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
	if l >= 65536 || r >= 65536 {
		return 0, false, Err533
	}
	return OpMingle(l, r), false, nil
}

func (e ExprMingle) ConstValue() (uint32, bool, bool) {
	l, _, lIsConst := e[0].ConstValue()
	r, _, rIsConst := e[1].ConstValue()
	if !lIsConst || l >= 65536 || !rIsConst || r >= 65536 {
		return 0, true, false
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

func (e ExprSelect) ConstValue() (uint32, bool, bool) {
	l, _, lIsConst := e[0].ConstValue()
	r, _, rIsConst := e[1].ConstValue()
	if !lIsConst || !rIsConst {
		return 0, true, false
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

func (e ExprAnd) ConstValue() (uint32, bool, bool) {
	v, is16, isConst := e[0].ConstValue()
	if !isConst {
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

func (e ExprOr) ConstValue() (uint32, bool, bool) {
	v, is16, isConst := e[0].ConstValue()
	if !isConst {
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

func (e ExprXor) ConstValue() (uint32, bool, bool) {
	v, is16, isConst := e[0].ConstValue()
	if !isConst {
		return 0, is16, false
	}
	if is16 {
		return OpXor16(v), true, true
	} else {
		return OpXor32(v), false, true
	}
}
