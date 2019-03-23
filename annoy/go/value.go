package main

type Value struct {
	version int
	stack   []*Value
}

func (v *Value) Size() int {
	size := 1
	for _, sv := range v.stack {
		size += sv.Size()
	}
	return size
}

func (v *Value) TryPush(pv *Value) bool {
	if len(v.stack) > 0 && pv.Size() > v.stack[len(v.stack)-1].Size() {
		return false
	}
	pv.version++
	v.stack = append(v.stack, pv)
	return true
}

func (v *Value) Push(pv *Value) bool {
	ok := true
	for i, sv := range v.stack {
		if pv.Size() > sv.Size() {
			v.stack = v.stack[:i]
			ok = false
			break
		}
	}
	pv.version++
	v.stack = append(v.stack, pv)
	return ok
}

func (v *Value) Pop() *Value {
	if len(v.stack) == 0 {
		return nil
	}
	pv := v.stack[len(v.stack)-1]
	v.stack = v.stack[:len(v.stack)-1]
	pv.version++
	return pv
}

func (v *Value) Version() int {
	return v.version
}
