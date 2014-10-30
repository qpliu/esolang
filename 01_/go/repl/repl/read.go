package repl

import (
	"io"
)

type reader struct {
	buffer     [128]byte
	start, end int
	tokenizer  Tokenizer
	in         io.Reader
}

func newReader(tokenizer Tokenizer, in io.Reader) *reader {
	return &reader{tokenizer: tokenizer, in: in}
}

func (r *reader) read() error {
	for {
		for r.start < r.end {
			b := r.buffer[r.start]
			r.start++
			if err := r.tokenizer.Read(b); err != nil {
				return err
			}
			if b == 0x0a {
				return nil
			}
		}
		r.start = 0
		if end, err := r.in.Read(r.buffer[:]); err != nil {
			return err
		} else {
			r.end = end
		}
	}
}
