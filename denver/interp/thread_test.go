package main

import (
	"sync"
	"testing"
)

func TestThread(t *testing.T) {
	a := NewThread()
	b := NewThread()
	var null *Thread

	if null.Send(a, b) != MessageResultExited {
		t.Errorf("null.Send")
	}
	if null.Notify() != MessageResultExited {
		t.Errorf("null.Send")
	}
	null.Exit()

	if a.Send(null, b) != MessageResultSuccess {
		t.Errorf("a.Send")
	}
	if a.Send(a, b) != MessageResultBlocked {
		t.Errorf("a.Send")
	}
	if a.Notify() != MessageResultSuccess {
		t.Errorf("a.Notify")
	}
	if _, result := a.Dequeue([]*Thread{a}); result != MessageResultBlocked {
		t.Errorf("a.Dequeue")
	}
	if _, result := a.Dequeue([]*Thread{a, nil}); result != MessageResultBlocked {
		t.Errorf("a.Dequeue")
	}
	if _, result := a.Dequeue([]*Thread{nil}); result != MessageResultExited {
		t.Errorf("a.Dequeue")
	}
	if msg, result := a.Dequeue([]*Thread{a, b}); result != MessageResultSuccess {
		t.Errorf("a.Dequeue")
	} else if msg[0] != nil {
		t.Errorf("a.Dequeue")
	} else if msg[1] != b {
		t.Errorf("a.Dequeue")
	}

	if _, result := a.Dequeue([]*Thread{}); result != MessageResultBlocked {
		t.Errorf("a.Dequeue")
	}
	if _, result := a.Dequeue([]*Thread{nil}); result != MessageResultExited {
		t.Errorf("a.Dequeue")
	}
	if _, result := a.Dequeue([]*Thread{nil, b}); result != MessageResultBlocked {
		t.Errorf("a.Dequeue")
	}

	if a.Send(a, b) != MessageResultSuccess {
		t.Errorf("a.Send")
	}
	if msg, result := a.Dequeue([]*Thread{}); result != MessageResultSuccess {
		t.Errorf("a.Dequeue")
	} else if msg[0] != a {
		t.Errorf("a.Dequeue")
	} else if msg[1] != b {
		t.Errorf("a.Dequeue")
	}

	if b.Send(null, a) != MessageResultSuccess {
		t.Errorf("b.Send")
	}
	b.Exit()
	if _, result := a.Dequeue([]*Thread{nil, b}); result != MessageResultExited {
		t.Errorf("a.Dequeue")
	}
	if b.Send(null, b) != MessageResultExited {
		t.Errorf("b.Send")
	}
	b.Exit()
}

func testThreadSend(recipient, msg, self *Thread) {
	for {
		switch recipient.Send(msg, self) {
		case MessageResultSuccess:
			return
		case MessageResultBlocked:
			self.Wait()
		case MessageResultExited:
			panic("testThreadSend:MessageResultExited")
		default:
			panic("testThreadSend:MessageResult")
		}
	}
}

func testThreadReceive(self *Thread) [2]*Thread {
	for {
		switch msg, status := self.Dequeue([]*Thread{}); status {
		case MessageResultSuccess:
			return msg
		case MessageResultBlocked:
			self.Wait()
		case MessageResultExited:
			panic("testThreadReceive:MessageResultExited")
		default:
			panic("testThreadReceive:MessageResult")
		}
	}
}

func TestThreadConcurrent(t *testing.T) {
	wg := sync.WaitGroup{}

	echo := NewThread()
	wg.Add(1)
	go func(self *Thread) {
		defer self.Exit()
		defer wg.Done()
		for _ = range []int{1, 2, 3} {
			msg := testThreadReceive(self)
			testThreadSend(msg[1], msg[0], self)
		}
	}(echo)

	a := NewThread()
	testThreadSend(echo, nil, a)
	r := testThreadReceive(a)
	if r[0] != nil {
		t.Errorf("a.Dequeue")
	}
	if r[1] != echo {
		t.Errorf("a.Dequeue")
	}

	testThreadSend(echo, a, a)
	r = testThreadReceive(a)
	if r[0] != a {
		t.Errorf("a.Dequeue")
	}
	if r[1] != echo {
		t.Errorf("a.Dequeue")
	}

	testThreadSend(echo, echo, a)
	r = testThreadReceive(a)
	if r[0] != echo {
		t.Errorf("a.Dequeue")
	}
	if r[1] != echo {
		t.Errorf("a.Dequeue")
	}
	wg.Wait()
	if echo.Send(echo, a) != MessageResultExited {
		t.Errorf("echo.Send")
	}
}
