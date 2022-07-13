package main

import (
	"io"
	"sync"
)

type Queue interface {
	Send(bit bool) bool
	Receive() (bool, bool)
	Close()
}

type ioQ struct {
	r    io.Reader
	w    io.Writer
	rBuf byte
	wBuf byte
	rBit byte
	wBit byte
}

func NewIOQueue(r io.Reader, w io.Writer) Queue {
	return &ioQ{r: r, w: w}
}

func (q *ioQ) Send(bit bool) bool {
	if q.wBit == 0 {
		q.wBit = 1
	} else {
		q.wBit <<= 1
	}
	if bit {
		q.wBuf |= q.wBit
	}
	if q.wBit == 128 {
		buf := []byte{q.wBuf}
		q.w.Write(buf)
		q.wBit = 0
		q.wBuf = 0
	}
	return true
}

func (q *ioQ) Receive() (bool, bool) {
	if q.rBit == 0 {
		buf := []byte{0}
		if _, err := q.r.Read(buf); err != nil {
			return false, false
		}
		q.rBit = 1
		q.rBuf = buf[0]
	} else {
		q.rBit <<= 1
	}
	bit := q.rBit&q.rBuf != 0
	if q.rBit == 128 {
		q.rBit = 0
	}
	return bit, true
}

func (q *ioQ) Close() {
}

type fullQ [2]*halfQ

func NewQueue() (Queue, Queue) {
	up := &halfQ{
		cond:  sync.NewCond(&sync.Mutex{}),
		empty: true,
		open:  true,
	}
	down := &halfQ{
		cond:  sync.NewCond(&sync.Mutex{}),
		empty: true,
		open:  true,
	}
	return fullQ{up, down}, fullQ{down, up}
}

func (q fullQ) Send(bit bool) bool {
	return q[0].send(bit)
}

func (q fullQ) Receive() (bool, bool) {
	return q[1].receive()
}

func (q fullQ) Close() {
	q[0].close()
	q[1].close()
}

type halfQ struct {
	cond  *sync.Cond
	empty bool
	bit   bool
	open  bool
}

func (q *halfQ) send(bit bool) bool {
	q.cond.L.Lock()
	defer q.cond.L.Unlock()
	for !q.empty && q.open {
		q.cond.Wait()
	}
	if !q.open {
		return false
	}
	q.empty = false
	q.bit = bit
	q.cond.Broadcast()
	return true
}

func (q *halfQ) receive() (bool, bool) {
	q.cond.L.Lock()
	defer q.cond.L.Unlock()
	for q.empty && q.open {
		q.cond.Wait()
	}
	if q.empty {
		return false, false
	}
	q.empty = true
	q.cond.Broadcast()
	return q.bit, true // return open=true for the last bit if closed
}

func (q *halfQ) close() {
	q.cond.L.Lock()
	defer q.cond.L.Unlock()
	q.open = false
	q.cond.Broadcast()
}
