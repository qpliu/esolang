package main

import (
	"io"
)

type Bitlist interface {
	Nil() bool
	Bit() bool
	Next() Bitlist
}

var NilBitlist = NewLiteralBitlist([]bool{})

type LiteralBitlist struct {
	bits []bool
	next *LiteralBitlist
}

func NewLiteralBitlist(bits []bool) *LiteralBitlist {
	result := new(LiteralBitlist)
	result.bits = bits
	return result
}

func (l *LiteralBitlist) Nil() bool {
	return len(l.bits) == 0
}

func (l *LiteralBitlist) Bit() bool {
	if len(l.bits) == 0 {
		panic("nil.Bit()")
	}
	return l.bits[0]
}

func (l *LiteralBitlist) Next() Bitlist {
	if len(l.bits) == 0 {
		panic("nil.Next()")
	}
	if l.next == nil {
		l.next = NewLiteralBitlist(l.bits[1:])
	}
	return l.next
}

type ReaderBitlist struct {
	index, byte uint
	reader      io.Reader
	next        *ReaderBitlist
}

func NewReaderBitlist(reader io.Reader) *ReaderBitlist {
	result := new(ReaderBitlist)
	result.reader = reader
	return result
}

func (r *ReaderBitlist) force() {
	if r.index == 0 {
		buf := []byte{0}
		switch n, err := r.reader.Read(buf); err {
		case nil, io.EOF:
			if n == 0 {
				r.index = 255
			} else {
				r.index = 128
				r.byte = uint(buf[0])
			}
		default:
			panic(err.Error())
		}
	}
}

func (r *ReaderBitlist) Nil() bool {
	r.force()
	return r.index == 255
}

func (r *ReaderBitlist) Bit() bool {
	r.force()
	if r.index == 255 {
		panic("nil.Bit()")
	}
	return r.byte&r.index != 0
}

func (r *ReaderBitlist) Next() Bitlist {
	r.force()
	if r.index == 255 {
		panic("nil.Next()")
	}
	if r.next == nil {
		r.next = new(ReaderBitlist)
		r.next.index = r.index >> 1
		r.next.byte = r.byte
		r.next.reader = r.reader
	}
	return r.next
}

func WriteBits(writer io.Writer, bitlist Bitlist) {
	buf := []byte{0}
	var index byte = 128
	for !bitlist.Nil() {
		if bitlist.Bit() {
			buf[0] = buf[0] | index
		}
		index = index >> 1
		if index == 0 {
			_, err := writer.Write(buf)
			if err != nil {
				panic(err.Error())
			}
			buf[0] = 0
			index = 128
		}
		bitlist = bitlist.Next()
	}
}
