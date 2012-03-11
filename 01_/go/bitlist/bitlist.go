package bitlist

import (
	"io"
)

type Bitlist interface {
	Nil() bool
	Bit() bool
	Next() Bitlist
}

var NilBitlist = &literalBitlist{}

type literalBitlist struct {
	bits []bool
	next *literalBitlist
}

func NewLiteralBitlist(bits []bool) Bitlist {
	return &literalBitlist{bits: bits}
}

func (l *literalBitlist) Nil() bool {
	return len(l.bits) == 0
}

func (l *literalBitlist) Bit() bool {
	if len(l.bits) == 0 {
		panic("nil.Bit()")
	}
	return l.bits[0]
}

func (l *literalBitlist) Next() Bitlist {
	if len(l.bits) == 0 {
		panic("nil.Next()")
	}
	if l.next == nil {
		l.next = &literalBitlist{bits: l.bits[1:]}
	}
	return l.next
}

type readerBitlist struct {
	index  uint8
	buf    [1]uint8
	reader io.Reader
	next   *readerBitlist
}

func NewReaderBitlist(reader io.Reader) Bitlist {
	return &readerBitlist{reader: reader}
}

func (r *readerBitlist) force() {
	if r.index == 0 {
		switch n, err := r.reader.Read(r.buf[:]); err {
		case nil, io.EOF:
			if n == 0 {
				r.index = 255
			} else {
				r.index = 128
			}
		default:
			panic(err.Error())
		}
	}
}

func (r *readerBitlist) Nil() bool {
	r.force()
	return r.index == 255
}

func (r *readerBitlist) Bit() bool {
	r.force()
	if r.index == 255 {
		panic("nil.Bit()")
	}
	return r.buf[0]&r.index != 0
}

func (r *readerBitlist) Next() Bitlist {
	r.force()
	if r.index == 255 {
		panic("nil.Next()")
	}
	if r.next == nil {
		r.next = &readerBitlist{index: r.index >> 1, buf: r.buf, reader: r.reader}
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
