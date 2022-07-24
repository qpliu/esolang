package main

import (
	"sync"
)

type MessageResult int

const (
	MessageResultSuccess MessageResult = iota
	MessageResultBlocked
	MessageResultExited
)

type Thread struct {
	lock        sync.Cond
	notified    bool
	exited      bool
	recvQueue   map[*Thread]*Thread
	terminating bool
	terminated  bool
}

func NewThread() *Thread {
	return &Thread{
		lock:      sync.Cond{L: &sync.Mutex{}},
		recvQueue: map[*Thread]*Thread{},
	}
}

func (t *Thread) Send(message, sender *Thread) MessageResult {
	if t == nil {
		return MessageResultExited
	}

	t.lock.L.Lock()
	defer t.lock.L.Unlock()

	if t.exited {
		return MessageResultExited
	} else if _, ok := t.recvQueue[sender]; ok {
		return MessageResultBlocked
	} else {
		t.recvQueue[sender] = message
		t.lock.Signal()
		return MessageResultSuccess
	}
}

func (t *Thread) Exit() {
	if t == nil {
		return
	}

	notifyList := []*Thread{}
	func() {
		t.lock.L.Lock()
		defer t.lock.L.Unlock()
		if t.exited {
			return
		}

		t.exited = true
		for k := range t.recvQueue {
			notifyList = append(notifyList, k)
		}
	}()
	for _, k := range notifyList {
		k.Notify()
	}
}

func (t *Thread) Notify() MessageResult {
	if t == nil {
		return MessageResultExited
	}

	t.lock.L.Lock()
	defer t.lock.L.Unlock()

	if t.exited {
		return MessageResultExited
	}
	t.notified = true
	t.lock.Signal()
	return MessageResultSuccess
}

func (t *Thread) Dequeue(senders []*Thread) ([2]*Thread, MessageResult) {
	if t == nil || t.exited {
		panic("Thread.Dequeue")
	}

	item := [2]*Thread{nil, nil}
	result := MessageResultBlocked
	func() {
		t.lock.L.Lock()
		defer t.lock.L.Unlock()

		if len(senders) == 0 {
			for sender, message := range t.recvQueue {
				item[0] = message
				item[1] = sender
				result = MessageResultSuccess
				delete(t.recvQueue, sender)
				return
			}
		} else {
			for _, sender := range senders {
				if message, ok := t.recvQueue[sender]; ok {
					item[0] = message
					item[1] = sender
					result = MessageResultSuccess
					delete(t.recvQueue, sender)
					return
				}
			}
		}
	}()
	if result == MessageResultSuccess {
		item[1].Notify()
		return item, MessageResultSuccess
	}
	if len(senders) == 0 {
		return item, MessageResultBlocked
	}
	result = MessageResultExited
	for _, sender := range senders {
		if sender.Notify() != MessageResultExited {
			result = MessageResultBlocked
		}
	}
	return item, result
}

func (t *Thread) SignalTerminating() {
	t.lock.L.Lock()
	defer t.lock.L.Unlock()
	t.terminating = true
	t.lock.Signal()
}

func (t *Thread) IsTerminating() bool {
	t.lock.L.Lock()
	defer t.lock.L.Unlock()
	return t.terminating
}

func (t *Thread) Terminated() {
	t.lock.L.Lock()
	defer t.lock.L.Unlock()
	t.terminated = true
	t.lock.Signal()
}

func (t *Thread) WaitTerminated() {
	t.lock.L.Lock()
	defer t.lock.L.Unlock()
	for !t.terminated {
		t.lock.Wait()
	}
}

func (t *Thread) Wait() {
	if t == nil || t.exited {
		panic("Thread.Wait")
	}

	t.lock.L.Lock()
	defer t.lock.L.Unlock()
	if t.notified {
		t.notified = false
		return
	}
	t.lock.Wait()
	if t.notified {
		t.notified = false
	}
}
