package main

import (
	"bytes"
	"sync"
	"testing"
)

type testInput struct {
	bits  []bool
	index int
}

func (in *testInput) Bit() (bool, bool) {
	if in.index >= len(in.bits) {
		return false, false
	} else {
		return in.bits[in.index], true
	}
}

func (in *testInput) Next() {
	if in.index < len(in.bits) {
		in.index++
	}
}

type testOutput struct {
	bits []bool

	eofCond *sync.Cond
	eof     bool
}

func newTestOutput() *testOutput {
	return &testOutput{eofCond: sync.NewCond(&sync.Mutex{})}
}

func (out *testOutput) Write(bit bool) {
	out.bits = append(out.bits, bit)
}

func (out *testOutput) Check(t *testing.T, label string, bits ...bool) {
	if len(bits) != len(out.bits) {
		t.Errorf("%s: out check len %d != %d", label, len(bits), len(out.bits))
	} else {
		for i := range bits {
			if bits[i] != out.bits[i] {
				t.Errorf("%s: out check bit[%d]", label, i)
				break
			}
		}
	}
}

func TestReaderInput(t *testing.T) {
	in := ReaderInput(bytes.NewBuffer([]byte{5}))
	for i, b := range []bool{true, false, true, false, false, false, false, false} {
		if bit, ok := in.Bit(); b != bit || !ok {
			t.Errorf("in[%d] %v,%v", i, bit, ok)
		}
		in.Next()
	}
	if _, ok := in.Bit(); ok {
		t.Errorf("in eof")
	}
}

func TestWriterOutput(t *testing.T) {
	buf := bytes.Buffer{}
	out := WriterOutput(&buf)
	out.Write(true)
	out.Write(false)
	out.Write(true)
	out.Write(false)
	out.Write(false)
	out.Write(false)
	out.Write(false)
	out.Write(false)
	data := buf.Bytes()
	if len(data) != 1 {
		t.Errorf("out len %d != 1", len(data))
	} else if data[0] != 5 {
		t.Errorf("out %d != 5", data[0])
	}
}

func testSystem(t *testing.T, testOut Output) {
	sys := SystemThread(&testInput{bits: []bool{true, false, true, false, false, false, false, false}}, testOut)

	a := NewThread()

	testThreadSend(sys, nil, a)
	r := testThreadReceive(a)
	if r[0] != nil || r[1] != sys {
		t.Errorf("receive")
	}

	testThreadSend(sys, sys, a)
	r = testThreadReceive(a)
	if r[0] == nil || r[0] == sys || r[0] == a || r[1] != sys {
		t.Errorf("receive")
	}
	in := r[0]

	testThreadSend(sys, sys, a)
	r = testThreadReceive(a)
	if r[0] != in || r[1] != sys {
		t.Errorf("receive")
	}

	testThreadSend(sys, in, a)
	r = testThreadReceive(a)
	if r[0] == nil || r[0] == sys || r[0] == in || r[0] == a || r[1] != sys {
		t.Errorf("receive")
	}
	out := r[0]

	testThreadSend(sys, out, a)
	r = testThreadReceive(a)
	if r[0] != nil || r[1] != sys {
		t.Errorf("receive")
	}

	testThreadSend(in, a, a)
	r = testThreadReceive(a)
	if r[0] != in || r[1] != in {
		t.Errorf("receive 1")
	}

	testThreadSend(in, a, a)
	r = testThreadReceive(a)
	if r[0] != nil || r[1] != in {
		t.Errorf("receive 0")
	}

	testThreadSend(in, a, a)
	r = testThreadReceive(a)
	if r[0] != in || r[1] != in {
		t.Errorf("receive 1")
	}

	testThreadSend(in, a, a)
	r = testThreadReceive(a)
	if r[0] != nil || r[1] != in {
		t.Errorf("receive 0")
	}

	testThreadSend(in, a, a)
	r = testThreadReceive(a)
	if r[0] != nil || r[1] != in {
		t.Errorf("receive 0")
	}

	testThreadSend(in, a, a)
	r = testThreadReceive(a)
	if r[0] != nil || r[1] != in {
		t.Errorf("receive 0")
	}

	testThreadSend(in, a, a)
	r = testThreadReceive(a)
	if r[0] != nil || r[1] != in {
		t.Errorf("receive 0")
	}

	testThreadSend(in, a, a)
	r = testThreadReceive(a)
	if r[0] != nil || r[1] != in {
		t.Errorf("receive 0")
	}

	if in.Send(a, a) != MessageResultSuccess {
		t.Errorf("in.Send")
	}
	if in.Notify() != MessageResultExited {
		a.Wait()
		if in.Notify() != MessageResultExited {
			t.Errorf("in.Notify")
		}
	}

	testThreadSend(out, a, a)
	testThreadSend(out, nil, a)
	testThreadSend(out, a, a)
	testThreadSend(out, nil, a)
	testThreadSend(out, nil, a)
	testThreadSend(out, nil, a)
	testThreadSend(out, nil, a)
	testThreadSend(out, nil, a)

	sys.SignalTerminating()
	sys.WaitTerminated()
}

func TestSystem(t *testing.T) {
	testOut := newTestOutput()
	testSystem(t, testOut)
	testOut.Check(t, "output", true, false, true, false, false, false, false, false)

	buf := bytes.Buffer{}
	writerOut := WriterOutput(&buf)
	testSystem(t, writerOut)
	data := buf.Bytes()
	if len(data) != 1 {
		t.Errorf("out len %d != 1", len(data))
	} else if data[0] != 5 {
		t.Errorf("out %d != 5", data[0])
	}
}
