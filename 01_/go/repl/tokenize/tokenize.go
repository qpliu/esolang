package tokenize

import (
	"bytes"
)

type Tokenizer struct {
	buffer                       bytes.Buffer
	tokens                       []string
	precomment, comment, literal bool
}

func (t *Tokenizer) closeBuffer() {
	if t.buffer.Len() > 0 {
		t.tokens = append(t.tokens, t.buffer.String())
		t.buffer.Reset()
	}
}

func (t *Tokenizer) startLiteral() {
	if !t.literal {
		t.closeBuffer()
		t.literal = true
	}
}

func (t *Tokenizer) endLiteral() {
	if t.literal {
		t.closeBuffer()
		t.literal = false
	}
}

func (t *Tokenizer) endIdentifier() {
	if !t.literal {
		t.closeBuffer()
	}
}

func (t *Tokenizer) Tokens() (tokens []string) {
	tokens = t.tokens
	t.tokens = nil
	return
}

func (t *Tokenizer) Read(b byte) error {
	if t.comment {
		t.comment = b != 0x0a
		return nil
	}
	if t.precomment {
		t.precomment = false
		if b == 0x3d {
			t.comment = true
			return nil
		}
		t.closeBuffer()
		t.tokens = append(t.tokens, "=")
	}
	switch b {
	case 0x09, 0x0a, 0x0c, 0x0d, 0x20:
		t.endIdentifier()
		return nil
	case 0x2e:
		t.closeBuffer()
		t.tokens = append(t.tokens, ".")
		return nil
	case 0x30, 0x31:
		t.startLiteral()
		return t.buffer.WriteByte(b)
	case 0x5f:
		t.startLiteral()
		if err := t.buffer.WriteByte(b); err != nil {
			return err
		}
		t.endLiteral()
		return nil
	case 0x3d:
		t.precomment = true
		return nil
	default:
		t.endLiteral()
		return t.buffer.WriteByte(b)
	}
}

func (t *Tokenizer) Finalize() {
	t.closeBuffer()
	if t.precomment {
		t.tokens = append(t.tokens, "=")
	}
	t.precomment = false
	t.comment = false
	t.literal = false
}
