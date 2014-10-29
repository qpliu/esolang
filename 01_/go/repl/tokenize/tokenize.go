package tokenize

import (
	"bytes"
)

func Tokenize(in <-chan byte, out chan<- string) {
	var buffer bytes.Buffer
	literal := false
	resetBuffer := func() {
		if buffer.Len() == 0 {
			return
		}
		out <- buffer.String()
		buffer.Reset()
	}
	startLiteral := func() {
		if literal {
			return
		}
		resetBuffer()
		literal = true
	}
	endLiteral := func() {
		if !literal {
			return
		}
		resetBuffer()
		literal = false
	}
	endIdentifier := func() {
		if literal {
			return
		}
		resetBuffer()
	}
	writeBuffer := func(b byte) {
		if err := buffer.WriteByte(b); err != nil {
			panic(err)
		}
	}
	precomment := false
	comment := false
	for b := range in {
		if comment {
			comment = b != 0x0a
			continue
		}
		if precomment {
			precomment = false
			if b == 0x3d {
				comment = true
				continue
			} else {
				resetBuffer()
				out <- "="
			}
		}
		switch b {
		case 0x09, 0x0a, 0x0c, 0x0d, 0x20:
			endIdentifier()
		case 0x2e:
			resetBuffer()
			out <- "."
		case 0x30, 0x31:
			startLiteral()
			writeBuffer(b)
		case 0x5f:
			startLiteral()
			writeBuffer(b)
			endLiteral()
		case 0x3d:
			precomment = true
		default:
			endLiteral()
			writeBuffer(b)
		}
	}
	resetBuffer()
	if precomment {
		out <- "="
	}
}
