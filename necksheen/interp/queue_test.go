package main

import (
	"bytes"
	"strings"
	"sync"
	"testing"
)

func TestIOQueue(t *testing.T) {
	const testData = "test data"
	out := &bytes.Buffer{}
	in := bytes.NewBufferString(testData)
	ioq := NewIOQueue(in, out)
	for {
		bit, open := ioq.Receive()
		if !open {
			break
		}
		open = ioq.Send(bit)
		if !open {
			t.Errorf("ioq.Send")
		}
	}
	ioq.Close()
	result := out.String()
	if result != testData {
		t.Errorf("result:%s", result)
	}
}

func TestQueue(t *testing.T) {
	testData := []bool{true, false, false, true}

	q1, q2 := NewQueue()
	for i, b := range testData {
		open := q1.Send(b)
		if !open {
			t.Errorf("q1.Send:%d", i)
		}
		bit, open := q2.Receive()
		if bit != b || !open {
			t.Errorf("q2.Receive:%d", i)
		}
		open = q2.Send(b)
		if !open {
			t.Errorf("q2.Send:%d", i)
		}
		bit, open = q1.Receive()
		if bit != b || !open {
			t.Errorf("q1.Receive:%d", i)
		}
	}
	q1.Close()
	open := q1.Send(true)
	if open {
		t.Errorf("q1.Send")
	}
	_, open = q2.Receive()
	if open {
		t.Errorf("q1.Send")
	}
}

func TestConcurrentQueue(t *testing.T) {
	var wg sync.WaitGroup

	const testData = "test data"
	out := &bytes.Buffer{}
	in := bytes.NewBufferString(testData)
	ioq := NewIOQueue(in, out)

	wg.Add(2)

	q1, q2 := NewQueue()
	xfer := func(qin Queue, qout Queue) {
		i := 0
		for {
			bit, open := qin.Receive()
			if !open {
				break
			}
			qout.Send(bit)
			i++
		}
		qout.Close()
		wg.Done()
	}
	go xfer(ioq, q1)
	go xfer(q2, ioq)

	wg.Wait()

	result := out.String()
	if result != testData {
		t.Errorf("result:%s", result)
	}
}

type TestingQueue struct {
	input string
	index int

	output strings.Builder
}

func NewTestingQueue(input string) *TestingQueue {
	return &TestingQueue{input: input}
}

func (q *TestingQueue) Output() string {
	return q.output.String()
}

func (q *TestingQueue) Send(bit bool) bool {
	if bit {
		q.output.WriteRune('1')
	} else {
		q.output.WriteRune('0')
	}
	return true
}

func (q *TestingQueue) Receive() (bool, bool) {
	if q.index >= len(q.input) {
		return false, false
	}
	q.index++
	return q.input[q.index-1]&1 != 0, true
}

func (q *TestingQueue) Close() {
}

func TestTestingQueue(t *testing.T) {
	const testData = "0011101"
	q := NewTestingQueue(testData)
	for {
		bit, open := q.Receive()
		if !open {
			break
		}
		open = q.Send(bit)
		if !open {
			t.Errorf("TestingQueue.Send")
		}
	}
	q.Close()
	result := q.Output()
	if result != testData {
		t.Errorf("result:%s", result)
	}
}
