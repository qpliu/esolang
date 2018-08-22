package intercal

import (
	"bytes"
	"io"
	"testing"
)

func testTokenize(src string) ([]*Token, error) {
	tokens := []*Token{}
	tokenizer := NewTokenizer("()", bytes.NewBufferString(src))
	for {
		token, err := tokenizer.Next()
		if token != nil {
			tokens = append(tokens, token)
		}
		if err != nil {
			if err == io.EOF {
				err = nil
			}
			return tokens, err
		}
	}
}

func testTokenizeTypes(t *testing.T, src string, tokenTypes []TokenType) {
	tokens, err := testTokenize(src)
	if err != nil {
		t.Errorf("Unexpected error=%s", err.Error())
	}
	if len(tokens) != len(tokenTypes) {
		t.Errorf("Unexpected token count=%d expected=%d", len(tokens), len(tokenTypes))
		return
	}
	for i, token := range tokens {
		if token.Type != tokenTypes[i] {
			t.Errorf("Unexpected token type[%d]=%d expected=%d", i, token.Type, tokenTypes[i])
		}
	}
}

func testTokenizeNumbers(t *testing.T, src string, values []uint16) {
	tokens, err := testTokenize(src)
	if err != nil {
		t.Errorf("Unexpected error=%s", err.Error())
	}
	if len(tokens) != len(values) {
		t.Errorf("Unexpected token count=%d expected=%d", len(tokens), len(values))
		return
	}
	for i, token := range tokens {
		if token.NumberValue != values[i] {
			t.Errorf("Unexpected token value[%d]=%d expected=%d", i, token.NumberValue, values[i])
		}
	}
}

func testTokenizeStrings(t *testing.T, src string, values []string) {
	tokens, err := testTokenize(src)
	if err != nil {
		t.Errorf("Unexpected error=%s", err.Error())
	}
	if len(tokens) != len(values) {
		t.Errorf("Unexpected token count=%d expected=%d", len(tokens), len(values))
		return
	}
	for i, token := range tokens {
		if token.StringValue != values[i] {
			t.Errorf("Unexpected token value[%d]=%s expected=%s", i, token.StringValue, values[i])
		}
	}
}

func TestTokenizer(t *testing.T) {
	testTokenizeTypes(t, "", []TokenType{})
	testTokenizeTypes(t, "DO :1 <- #0$#256", []TokenType{
		TokenDo,
		TokenTwoSpot,
		TokenNumber,
		TokenAngle,
		TokenWorm,
		TokenMesh,
		TokenNumber,
		TokenBigMoney,
		TokenMesh,
		TokenNumber,
	})
	testTokenizeNumbers(t, "DO :1 <- #0$#256", []uint16{
		0,
		0,
		1,
		0,
		0,
		0,
		0,
		0,
		0,
		256,
	})
	testTokenizeStrings(t, "DO :1 <- #0$#256", []string{
		"DO ",
		":",
		"1 ",
		"<",
		"- ",
		"#",
		"0",
		"$",
		"#",
		"256",
	})
	testTokenizeTypes(t, "PLEASE NOTE THAT THIS LINE HAS NO EFFECT", []TokenType{
		TokenPlease,
		TokenNot,
		TokenString,
		TokenIn,
		TokenString,
	})
	testTokenizeStrings(t, "PLEASE NOTE THAT THIS LINE HAS NO EFFECT", []string{
		"PLEASE ",
		"NOT",
		"E THAT THIS L",
		"IN",
		"E HAS NO EFFECT",
	})
	testTokenizeTypes(t, "(12 \n 3 \n 4 \n 5 \n ) PLEASE GIVE UP", []TokenType{
		TokenWax,
		TokenNumber,
		TokenWane,
		TokenPlease,
		TokenGive,
		TokenUp,
	})
	testTokenizeNumbers(t, "(12 \n 3 \n 4 \n 5 \n ) PLEASE GIVE UP", []uint16{
		0,
		12345,
		0,
		0,
		0,
		0,
	})
	testTokenizeTypes(t, "DO ABSTAIN FROM ABSTAINING + GIVING UP", []TokenType{
		TokenDo,
		TokenAbstain,
		TokenFrom,
		TokenAbstaining,
		TokenIntersection,
		TokenGiving,
		TokenUp,
	})
}
