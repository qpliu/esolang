package repl

import (
	"io"
)

type Tokenizer interface {
	Tokens() []string
	Read(b byte) error
	Finalize()
}

type State interface {
	Prompt() string
	Eval(tokens []string) string
}

func Repl(in io.Reader, out io.Writer, state State, tokenizer Tokenizer) error {
	reader := newReader(tokenizer, in)
	for {
		if _, err := io.WriteString(out, state.Prompt()); err != nil {
			return err
		}
		if err := reader.read(); err != nil {
			if err == io.EOF {
				return nil
			}
			return err
		}
		if result := state.Eval(tokenizer.Tokens()); result != "" {
			if _, err := io.WriteString(out, result); err != nil {
				return err
			}
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		}
	}
}
