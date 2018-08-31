package intercal

import (
	"io"
	"os"
	"sort"
	"strconv"
)

type Tokenizer struct {
	r     io.Reader
	atEOF bool

	buffer       []byte
	index        int
	tokenBuffer  []byte
	tokenIndexes []int

	f         *os.File
	filenames []string
	fileIndex int
}

func NewTokenizer(r io.Reader) *Tokenizer {
	return &Tokenizer{r: r}
}

func NewFileTokenizer(filenames []string) (*Tokenizer, error) {
	f, err := os.Open(filenames[0])
	if err != nil {
		return nil, err
	}
	return &Tokenizer{
		r:         f,
		f:         f,
		filenames: filenames,
		fileIndex: 0,
	}, nil
}

func (t *Tokenizer) Next() (*Token, error) {
	atTrailingWhitespace := false
	atTrailingDigit := false
	for {
		if !t.atEOF && (atTrailingWhitespace || atTrailingDigit || len(t.tokenBuffer) < maxTokenLen+1) {
			buf := []byte{0}
			if n, err := t.r.Read(buf); n == 0 {
				if err != io.EOF {
					return nil, err
				}
				if t.f != nil {
					t.f.Close()
					t.f = nil
				}
				if t.fileIndex+1 < len(t.filenames) {
					t.fileIndex++
					if f, err := os.Open(t.filenames[t.fileIndex]); err != nil {
						return nil, err
					} else {
						t.f = f
						t.r = f
					}
				} else {
					t.atEOF = true
				}
			} else {
				t.buffer = append(t.buffer, buf[0])
				t.index++
				if isSpace(buf[0]) {
					atTrailingWhitespace = true
				} else {
					t.tokenBuffer = append(t.tokenBuffer, buf[0])
					t.tokenIndexes = append(t.tokenIndexes, t.index-1)
					atTrailingWhitespace = false
					atTrailingDigit = isDigit(buf[0])
				}
				continue
			}
		}

		if len(t.tokenBuffer) == 0 {
			for _, b := range t.buffer {
				if !isSpace(b) {
					token := &Token{
						Type:        TokenString,
						StringValue: string(t.buffer),
					}
					t.buffer = t.buffer[:0]
					return token, nil
				}
			}
			return nil, io.EOF
		}

		if isDigit(t.tokenBuffer[0]) {
			if leadingJunkToken := t.leadingJunk(t.tokenIndexes[0]); leadingJunkToken != nil {
				return leadingJunkToken, nil
			}
			tokenLen := 1
			for tokenLen < len(t.tokenBuffer) && isDigit(t.tokenBuffer[tokenLen]) {
				tokenLen++
			}
			tokenType := TokenString
			numberValue := 0
			if n, err := strconv.Atoi(string(t.tokenBuffer[:tokenLen])); err != nil {
			} else if n < 65536 {
				tokenType = TokenNumber
				numberValue = n
			}
			token := t.nextToken(tokenLen)
			token.Type = tokenType
			token.NumberValue = uint16(numberValue)
			return token, nil
		}

		var tokenMatch *tokenTableEntry
	tokenTableLoop:
		for i := range tokenTable {
			if len(t.tokenBuffer) < len(tokenTable[i].Value) {
				// can happen on EOF
				continue
			}
			hasSpaces := false
			for i, v := range []byte(tokenTable[i].Value) {
				if v != t.tokenBuffer[i] {
					continue tokenTableLoop
				}
				if i > 0 && t.tokenIndexes[i] != t.tokenIndexes[i-1]+1 {
					hasSpaces = true
				}
			}
			if hasSpaces && tokenTable[i].NoSpaces {
				continue
			}
			tokenMatch = &tokenTable[i]
			break
		}
		if tokenMatch == nil {
			newLen := len(t.tokenBuffer) - 1
			copy(t.tokenBuffer, t.tokenBuffer[1:])
			copy(t.tokenIndexes, t.tokenIndexes[1:])
			t.tokenBuffer = t.tokenBuffer[:newLen]
			t.tokenIndexes = t.tokenIndexes[:newLen]
			continue
		}

		if leadingJunkToken := t.leadingJunk(t.tokenIndexes[0]); leadingJunkToken != nil {
			return leadingJunkToken, nil
		}

		token := t.nextToken(len(tokenMatch.Value))
		token.Type = tokenMatch.Type
		return token, nil
	}
}

func isDigit(b byte) bool {
	switch b {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return true
	default:
		return false
	}
}

func isSpace(b byte) bool {
	switch b {
	case ' ', '\t', '\f', '\r', '\n':
		return true
	default:
		return false
	}
}

func (t *Tokenizer) leadingJunk(tokenStartIndex int) *Token {
	leadingLen := tokenStartIndex - (t.index - len(t.buffer))
	lastNewline := -1
	lastJunk := -1
	for i := 0; i < leadingLen; i++ {
		if !isSpace(t.buffer[i]) {
			lastJunk = i
		} else if t.buffer[i] == '\n' {
			lastNewline = i
		}
	}
	if lastJunk >= 0 {
		if lastNewline > lastJunk {
			leadingLen = lastNewline + 1
		}
		junkToken := &Token{
			Type:        TokenString,
			StringValue: string(t.buffer[:leadingLen]),
		}
		newLen := len(t.buffer) - leadingLen
		copy(t.buffer, t.buffer[leadingLen:])
		t.buffer = t.buffer[:newLen]
		return junkToken
	}
	return nil
}

func (t *Tokenizer) nextToken(tokenBufferIndex int) *Token {
	tokenLen := t.tokenIndexes[tokenBufferIndex-1] - (t.index - len(t.buffer)) + 1
	for tokenLen < len(t.buffer) && isSpace(t.buffer[tokenLen]) {
		if t.buffer[tokenLen] == '\n' {
			tokenLen++
			break
		}
		tokenLen++
	}
	token := &Token{StringValue: string(t.buffer[:tokenLen])}

	newLen := len(t.buffer) - tokenLen
	copy(t.buffer, t.buffer[tokenLen:])
	t.buffer = t.buffer[:newLen]

	newLen = len(t.tokenBuffer) - tokenBufferIndex
	copy(t.tokenBuffer, t.tokenBuffer[tokenBufferIndex:])
	t.tokenBuffer = t.tokenBuffer[:newLen]
	copy(t.tokenIndexes, t.tokenIndexes[tokenBufferIndex:])
	t.tokenIndexes = t.tokenIndexes[:newLen]

	return token
}

type TokenType int

type Token struct {
	Type        TokenType
	NumberValue uint16
	StringValue string
}

func (t *Token) IsUnaryOp() bool {
	return t.Type == TokenAmpersand || t.Type == TokenBook || t.Type == TokenBookworm || t.Type == TokenWhat
}

const (
	TokenNumber TokenType = iota
	TokenString

	TokenSpot
	TokenTwoSpot
	TokenTail
	TokenHybrid
	TokenMesh
	TokenHalfMesh
	TokenSpark
	TokenBackSpark
	TokenWow
	TokenWhat
	TokenRabbitEars
	TokenRabbit
	TokenSpike
	TokenDoubleOhSeven
	TokenWorm
	TokenAngle
	TokenRightAngle
	TokenWax
	TokenWane
	TokenUTurn
	TokenUTurnBack
	TokenEmbrace
	TokenBracelet
	TokenSplat
	TokenAmpersand
	TokenBook
	TokenBookworm
	TokenBigMoney
	TokenChange
	TokenSqiggle
	TokenFlatWorm
	TokenIntersection
	TokenSlat
	TokenBackSlat
	TokenWhirlpool
	TokenHookworm
	TokenShark
	TokenBlotch

	TokenPlease
	TokenDo
	TokenNot
	TokenCalculating
	TokenNext
	TokenNexting
	TokenForget
	TokenForgetting
	TokenResume
	TokenResuming
	TokenStash
	TokenStashing
	TokenRetrieve
	TokenRetrieving
	TokenIgnore
	TokenIgnoring
	TokenRemember
	TokenRemembering
	TokenAbstain
	TokenAbstaining
	TokenFrom
	TokenReinstate
	TokenReinstating
	TokenGive
	TokenGiving
	TokenUp
	TokenWrite
	TokenWriting
	TokenIn
	TokenRead
	TokenReading
	TokenOut
	TokenSub
	TokenBy
	TokenThank
	TokenNaught
)

type tokenTableEntry struct {
	Value    string
	Type     TokenType
	NoSpaces bool
}

var tokenTable = []tokenTableEntry{
	{".", TokenSpot, true},
	{":", TokenTwoSpot, true},
	{",", TokenTail, true},
	{";", TokenHybrid, true},
	{"#", TokenMesh, true},
	{"=", TokenHalfMesh, true},
	{"'", TokenSpark, true},
	{"’", TokenSpark, true},
	{"`", TokenBackSpark, true},
	{"!", TokenWow, true},
	{"?", TokenWhat, true},
	{"\"", TokenRabbitEars, true},
	{"\"\u0008.", TokenRabbit, true},
	{"|", TokenSpike, true},
	{"%", TokenDoubleOhSeven, true},
	{"-", TokenWorm, true},
	{"<", TokenAngle, true},
	{">", TokenRightAngle, true},
	{"(", TokenWax, true},
	{")", TokenWane, true},
	{"[", TokenUTurn, true},
	{"]", TokenUTurnBack, true},
	{"{", TokenEmbrace, true},
	{"}", TokenBracelet, true},
	{"*", TokenSplat, true},
	{"&", TokenAmpersand, true},
	{"V", TokenBook, true},
	{"\u2228", TokenBook, true},
	{"V\u0008-", TokenBookworm, true},
	{"\u22bb", TokenBookworm, true},
	{"\u2200", TokenBookworm, true},
	{"$", TokenBigMoney, true},
	{"c\u0008/", TokenChange, true},
	{"c\u0008|", TokenChange, true},
	{"\u00a2", TokenChange, true},
	{"~", TokenSqiggle, true},
	{"_", TokenFlatWorm, true},
	{"+", TokenIntersection, true},
	{"/", TokenSlat, true},
	{"\\", TokenBackSlat, true},
	{"@", TokenWhirlpool, true},
	{"'\u0008-", TokenHookworm, true},
	{"^", TokenShark, true},
	{"#\u0008*\u0008[\u0008]", TokenBlotch, true},
	{"#\u0008I\u0008[\u0008]", TokenBlotch, true},

	{"PLEASE", TokenPlease, true},
	{"DO", TokenDo, true},
	{"NOT", TokenNot, false},
	{"N'T", TokenNot, false},
	{"CALCULATING", TokenCalculating, false},
	{"NEXT", TokenNext, false},
	{"NEXTING", TokenNexting, false},
	{"FORGET", TokenForget, false},
	{"FORGETTING", TokenForgetting, false},
	{"RESUME", TokenResume, false},
	{"RESUMING", TokenResuming, false},
	{"STASH", TokenStash, false},
	{"STASHING", TokenStashing, false},
	{"RETRIEVE", TokenRetrieve, false},
	{"RETRIEVING", TokenRetrieving, false},
	{"IGNORE", TokenIgnore, false},
	{"IGNORING", TokenIgnoring, false},
	{"REMEMBER", TokenRemember, false},
	{"REMEMBERING", TokenRemembering, false},
	{"ABSTAIN", TokenAbstain, false},
	{"ABSTAINING", TokenAbstaining, false},
	{"FROM", TokenFrom, false},
	{"REINSTATE", TokenReinstate, false},
	{"REINSTATING", TokenReinstating, false},
	{"GIVE", TokenGive, false},
	{"GIVING", TokenGiving, false},
	{"UP", TokenUp, false},
	{"WRITE", TokenWrite, false},
	{"WRITING", TokenWriting, false},
	{"IN", TokenIn, false},
	{"READ", TokenRead, false},
	{"READING", TokenReading, false},
	{"OUT", TokenOut, false},
	{"SUB", TokenSub, false},
	{"BY", TokenBy, false},
	{"THANK", TokenThank, false},
	{"NAUGHT", TokenNaught, false},
}
var maxTokenLen int

func init() {
	sort.Slice(tokenTable, func(i, j int) bool {
		return len(tokenTable[i].Value) > len(tokenTable[j].Value)
	})
	maxTokenLen = len(tokenTable[0].Value)
	if maxTokenLen < 5 {
		maxTokenLen = 5 // len("65535") == 5
	}
}
