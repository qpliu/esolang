package intercal

import (
	"bufio"
	"bytes"
	"errors"
	"io"
)

var (
	eol = errors.New("END OF LINE.")
)

type IntercalReader struct {
	*bufio.Reader
	binaryBuffer, binaryBufferIndex byte
}

func NewIntercalReader(r io.Reader) *IntercalReader {
	return &IntercalReader{Reader: bufio.NewReaderSize(r, 1)}
}

func (r *IntercalReader) inputToken(buf *bytes.Buffer) error {
	buf.Reset()
	for {
		b, err := r.Reader.ReadByte()
		if err != nil {
			return err
		}
		switch b {
		case ' ':
			continue
		case '\n':
			return eol
		default:
			buf.WriteByte(b)
			for {
				b, err := r.Reader.ReadByte()
				if err != nil {
					if err == io.EOF && buf.Len() > 0 {
						return nil
					}
					return err
				}
				switch b {
				case ' ', '\n':
					r.Reader.UnreadByte()
					return nil
				default:
					buf.WriteByte(b)
				}
			}
		}
	}
}

func (r *IntercalReader) inputDigit(buf *bytes.Buffer) (uint32, error) {
	if err := r.inputToken(buf); err != nil {
		return 0, err
	}
	digitName := buf.String()
	switch digitName {
	case "OH", "ZERO":
		return 0, nil
	case "ONE":
		return 1, nil
	case "TWO":
		return 2, nil
	case "THREE":
		return 3, nil
	case "FOUR":
		return 4, nil
	case "FIVE":
		return 5, nil
	case "SIX":
		return 6, nil
	case "SEVEN":
		return 7, nil
	case "EIGHT":
		return 8, nil
	case "NINE":
		return 9, nil
	default:
		return 0, Err579.WithMessage("WHAT BASE AND/OR LANGUAGE INCLUDES " + digitName + "?")
	}
}

func (r *IntercalReader) input(limit uint32, overflowErr error) (uint32, error) {
	if r == nil {
		return 0, Err562
	}
	var buf bytes.Buffer
	val, err := r.inputDigit(&buf)
	if err == eol || err == io.EOF {
		return 0, Err562
	} else if err != nil {
		return 0, err
	}
	for {
		digit, err := r.inputDigit(&buf)
		if err == eol {
			return uint32(val), nil
		} else if err == io.EOF {
			return 0, Err562
		} else if err != nil {
			return 0, err
		}
		if val > limit/10 || (val == limit/10 && digit > limit%10) {
			return 0, overflowErr
		}
		val = val*10 + digit
	}
}

func (r *IntercalReader) Input16() (uint16, error) {
	val, err := r.input(65535, Err275)
	return uint16(val), err
}

func (r *IntercalReader) Input32() (uint32, error) {
	return r.input(4294967295, Err533)
}

func (r *IntercalReader) InputBit() (bool, bool) {
	if r.binaryBufferIndex == 0 {
		r.binaryBufferIndex = 2
		b, err := r.Reader.ReadByte()
		if err != nil {
			return false, true
		}
		r.binaryBuffer = b
		return b&1 != 0, false
	}
	bit := r.binaryBuffer&r.binaryBufferIndex != 0
	r.binaryBufferIndex <<= 1
	return bit, false
}

func (r *IntercalReader) PeekBit() (bool, bool) {
	if r.binaryBufferIndex == 0 {
		return false, false
	}
	return r.binaryBuffer&r.binaryBufferIndex != 0, true
}
