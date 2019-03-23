package main

import (
	"testing"
)

func TestValue(t *testing.T) {
	z := Value{}
	if z.Size() != 1 || z.Version() != 0 {
		t.Errorf("z.Size=%d expected 1, z.Version=%d expected 0", z.Size(), z.Version())
	}
	if !z.TryPush(&Value{}) {
		t.Errorf("z.TryPush")
	}
	if z.Size() != 2 || z.Version() != 0 {
		t.Errorf("z.Size=%d expected 2, z.Version=%d expected 0", z.Size(), z.Version())
	}
	x := z.Pop()
	if z.Size() != 1 || z.Version() != 0 {
		t.Errorf("z.Size=%d expected 1, z.Version=%d expected 0", z.Size(), z.Version())
	}
	if x.Size() != 1 || x.Version() != 2 {
		t.Errorf("x.Size=%d expected 1, x.Version=%d expected 2", x.Size(), x.Version())
	}
	if z.Pop() != nil {
		t.Errorf("z.Pop")
	}
	if x.Pop() != nil {
		t.Errorf("x.Pop")
	}
	if !z.TryPush(&Value{}) {
		t.Errorf("z.TryPush")
	}
	if z.Size() != 2 || z.Version() != 0 {
		t.Errorf("z.Size=%d expected 2, z.Version=%d expected 0", z.Size(), z.Version())
	}
	if !x.Push(&Value{}) {
		t.Errorf("x.TryPush")
	}
	if x.Size() != 2 || x.Version() != 2 {
		t.Errorf("x.Size=%d expected 2, x.Version=%d expected 2", x.Size(), x.Version())
	}
	if z.TryPush(x) {
		t.Errorf("z.TryPush(x)")
	}
	if z.Size() != 2 || z.Version() != 0 {
		t.Errorf("z.Size=%d expected 2, z.Version=%d expected 0", z.Size(), z.Version())
	}
	if z.Push(x) {
		t.Errorf("z.Push(x)")
	}
	if z.Size() != 3 || z.Version() != 0 {
		t.Errorf("z.Size=%d expected 3, z.Version=%d expected 0", z.Size(), z.Version())
	}
	x = z.Pop()
	if z.Size() != 1 || z.Version() != 0 {
		t.Errorf("z.Size=%d expected 1, z.Version=%d expected 0", z.Size(), z.Version())
	}
	if x.Size() != 2 || x.Version() != 4 {
		t.Errorf("x.Size=%d expected 2, x.Version=%d expected 4", x.Size(), x.Version())
	}
}
