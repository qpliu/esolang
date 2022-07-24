package main

import (
	"io"
	"sync"
)

type Input interface {
	Bit() (bool, bool)
	Next()
}

type Output interface {
	Write(bool)
}

func SystemThread(r Input, w Output) *Thread {
	self := NewThread()
	go func() {
		in := InputThread(r)
		out := OutputThread(w)
		list := map[*Thread]*Thread{self: in, in: out}
		queue := [][2]*Thread{}
		for {
			status := MessageResultBlocked
			for i := range queue {
				if queue[i][1] == nil {
					continue
				}
				switch queue[i][1].Send(list[queue[i][0]], self) {
				case MessageResultSuccess:
					queue[i][1] = nil
					status = MessageResultSuccess
				case MessageResultExited:
					queue[i][1] = nil
				}
			}
			if status == MessageResultSuccess {
				continue
			}
			i := len(queue)
			for j := range queue {
				if queue[j][1] == nil {
					i = j
					break
				}
			}
			if i >= len(queue) {
				queue = append(queue, [2]*Thread{nil, nil})
			}
			queue[i], status = self.Dequeue([]*Thread{})
			switch status {
			case MessageResultSuccess:
				continue
			case MessageResultBlocked:
				if self.IsTerminating() {
					in.SignalTerminating()
					out.SignalTerminating()
					out.WaitTerminated()
					self.Terminated()
					return
				}
				self.Wait()
			case MessageResultExited:
				panic("MessageResultExited")
			default:
				panic("MessageResult")
			}
		}
	}()
	return self
}

func InputThread(r Input) *Thread {
	self := NewThread()
	go func() {
		queue := [][2]*Thread{}
		for {
			status := MessageResultBlocked
			for i := range queue {
				if queue[i][1] == nil {
					continue
				}
				bit, ok := r.Bit()
				if !ok {
					self.Exit()
					queue[i][1].Notify()
					self.Terminated()
					return
				}
				msg := self
				if !bit {
					msg = nil
				}
				switch queue[i][1].Send(msg, self) {
				case MessageResultSuccess:
					queue[i][1] = nil
					status = MessageResultSuccess
					r.Next()
				case MessageResultExited:
					queue[i][1] = nil
				}
			}
			if status == MessageResultSuccess {
				continue
			}
			i := len(queue)
			for j := range queue {
				if queue[j][1] == nil {
					i = j
					break
				}
			}
			if i >= len(queue) {
				queue = append(queue, [2]*Thread{nil, nil})
			}
			queue[i], status = self.Dequeue([]*Thread{})
			switch status {
			case MessageResultSuccess:
				continue
			case MessageResultBlocked:
				if self.IsTerminating() {
					self.Terminated()
					return
				}
				self.Wait()
			case MessageResultExited:
				panic("MessageResultExited")
			default:
				panic("MessageResult")
			}
		}

	}()
	return self
}

func OutputThread(w Output) *Thread {
	self := NewThread()
	go func() {
		for {
			msg, status := self.Dequeue([]*Thread{})
			switch status {
			case MessageResultSuccess:
				w.Write(msg[0] != nil)
			case MessageResultBlocked:
				if self.IsTerminating() {
					self.Terminated()
					return
				}
				self.Wait()
			case MessageResultExited:
				panic("MessageResultExited")
			default:
				panic("MessageResult")
			}
		}
	}()
	return self
}

type readerInput struct {
	r        io.Reader
	buf      [1]byte
	bitIndex int
	eof      bool
}

func (r *readerInput) Bit() (bool, bool) {
	if r.bitIndex&255 == 0 {
		_, err := r.r.Read(r.buf[:])
		if err != nil {
			return false, false
		}
		r.bitIndex = 1
	}
	return int(r.buf[0])&r.bitIndex != 0, true
}

func (r *readerInput) Next() {
	r.bitIndex <<= 1
}

func ReaderInput(r io.Reader) Input {
	return &readerInput{r: r}
}

type writerOutput struct {
	w        io.Writer
	buf      [1]byte
	bitIndex int

	eof     bool
	eofCond *sync.Cond
}

func (w *writerOutput) Write(bit bool) {
	if bit {
		w.buf[0] |= byte(w.bitIndex)
	}
	w.bitIndex <<= 1
	if w.bitIndex&255 == 0 {
		w.w.Write(w.buf[:])
		w.buf[0] = 0
		w.bitIndex = 1
	}
}

func WriterOutput(w io.Writer) Output {
	return &writerOutput{w: w, bitIndex: 1, eofCond: sync.NewCond(&sync.Mutex{})}
}
