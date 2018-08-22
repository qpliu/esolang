package intercal

import (
	"bufio"
	"bytes"
	"io"
	"os"
	"sort"
	"strings"
)

type Tokenizer struct {
	r *bufio.Reader

	f         *os.File
	filenames []string
	fileIndex int

	filename string

	line    string
	linenum int
	index   int
}

func NewTokenizer(filename string, r io.Reader) *Tokenizer {
	return &Tokenizer{
		r:        bufio.NewReader(r),
		filename: filename,
	}
}

func NewFileTokenizer(filenames []string) (*Tokenizer, error) {
	f, err := os.Open(filenames[0])
	if err != nil {
		return nil, err
	}
	return &Tokenizer{
		r:         bufio.NewReader(f),
		f:         f,
		filenames: filenames,
		fileIndex: 0,
		filename:  filenames[0],
	}, nil
}

func (t *Tokenizer) Next() (*Token, error) {
	leadingSpaces := bytes.Buffer{}
	for {
		if t.index >= len(t.line) {
			line, err := t.r.ReadString('\n')
			t.line = line
			if err != nil {
				if err != io.EOF {
					return nil, err
				}
				if line == "" {
					if t.f != nil {
						t.f.Close()
					}
					t.fileIndex++
					if t.fileIndex >= len(t.filenames) {
						return nil, err
					}
					f, err := os.Open(t.filenames[t.fileIndex])
					if err != nil {
						return nil, err
					}
					t.r = bufio.NewReader(f)
					t.f = f
					t.filename = t.filenames[t.fileIndex]
					t.linenum = 0
					continue
				}
			}
			t.linenum++
			t.index = 0
			continue
		}
		switch t.line[t.index] {
		case ' ', '\t', '\f', '\n', '\r', '\v':
			leadingSpaces.WriteByte(t.line[t.index])
			t.index++
			continue
		default:
		}
		firstNumber := strings.IndexAny(t.line[t.index:], "0123456789")
		tokenIndex := firstNumber
		tableIndex := -1
		for i, entry := range tokenTable {
			index := strings.Index(t.line[t.index:], entry.Value)
			if index >= 0 && (index < tokenIndex || tokenIndex < 0) {
				tokenIndex = index
				tableIndex = i
			}
		}
		if tokenIndex > 0 {
			colnum := t.index + 1
			stringValue := t.line[t.index : t.index+tokenIndex]
			t.index += tokenIndex
			return &Token{
				Filename:    t.filename,
				LineNum:     t.linenum,
				ColNum:      colnum,
				Type:        TokenString,
				StringValue: leadingSpaces.String() + stringValue + t.trailingSpaces(),
			}, nil
		}
		if tableIndex >= 0 {
			colnum := t.index + 1
			t.index += len(tokenTable[tableIndex].Value)
			return &Token{
				Filename:    t.filename,
				LineNum:     t.linenum,
				ColNum:      colnum,
				Type:        tokenTable[tableIndex].Type,
				StringValue: leadingSpaces.String() + tokenTable[tableIndex].Value + t.trailingSpaces(),
			}, nil
		} else if firstNumber > 0 {
			colnum := t.index + 1
			stringValue := t.line[t.index : t.index+firstNumber]
			t.index += firstNumber
			return &Token{
				Filename:    t.filename,
				LineNum:     t.linenum,
				ColNum:      colnum,
				Type:        TokenString,
				StringValue: leadingSpaces.String() + stringValue + t.trailingSpaces(),
			}, nil
		} else if firstNumber == 0 {
			linenum := t.linenum
			colnum := t.index + 1
			index := t.index
			value := 0
		loop:
			for {
				if index >= len(t.line) {
					line, err := t.r.ReadString('\n')
					t.line = line
					t.linenum++
					index = 0
					if err == io.EOF {
						break loop
					} else if err != nil {
						return nil, err
					}
				}
				switch t.line[index] {
				case ' ', '\t', '\f', '\n', '\r', '\v':
				case '0':
					if value < 65536 {
						value = value * 10
					}
				case '1':
					if value < 65536 {
						value = value*10 + 1
					}
				case '2':
					if value < 65536 {
						value = value*10 + 2
					}
				case '3':
					if value < 65536 {
						value = value*10 + 3
					}
				case '4':
					if value < 65536 {
						value = value*10 + 4
					}
				case '5':
					if value < 65536 {
						value = value*10 + 5
					}
				case '6':
					if value < 65536 {
						value = value*10 + 6
					}
				case '7':
					if value < 65536 {
						value = value*10 + 7
					}
				case '8':
					if value < 65536 {
						value = value*10 + 8
					}
				case '9':
					if value < 65536 {
						value = value*10 + 9
					}
				default:
					break loop
				}
				leadingSpaces.WriteByte(t.line[index])
				index++
			}
			t.index = index
			spacesBelongToNextLine := true
			for i := 0; i < index && spacesBelongToNextLine; i++ {
				switch t.line[i] {
				case ' ', '\t', '\f', '\n', '\r', '\v':
				default:
					spacesBelongToNextLine = false
				}
			}
			if spacesBelongToNextLine {
				leadingSpaces.Truncate(leadingSpaces.Len() - index)
				t.index = 0
			} else {
				leadingSpaces.WriteString(t.trailingSpaces())
			}
			tokenType := TokenNumber
			if value >= 65536 {
				tokenType = TokenString
				value = 0
			}
			return &Token{
				Filename:    t.filename,
				LineNum:     linenum,
				ColNum:      colnum,
				Type:        tokenType,
				NumberValue: uint16(value),
				StringValue: leadingSpaces.String(),
			}, nil
		} else {
			colnum := t.index + 1
			stringValue := t.line[t.index:]
			t.index = len(t.line)
			return &Token{
				Filename:    t.filename,
				LineNum:     t.linenum,
				ColNum:      colnum,
				Type:        TokenString,
				StringValue: leadingSpaces.String() + stringValue + t.trailingSpaces(),
			}, nil
		}
	}
}

func (t *Tokenizer) trailingSpaces() string {
	spaces := bytes.Buffer{}
	for {
		if t.index >= len(t.line) {
			return spaces.String()
		}
		switch t.line[t.index] {
		case ' ', '\t', '\f', '\n', '\r', '\v':
			spaces.WriteByte(t.line[t.index])
			t.index++
		default:
			return spaces.String()
		}
	}
}

type TokenType int

type Token struct {
	Filename string
	LineNum  int
	ColNum   int

	Type TokenType

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
)

type tokenTableEntry struct {
	Value string
	Type  TokenType
}

var tokenTable = []tokenTableEntry{
	{".", TokenSpot},
	{":", TokenTwoSpot},
	{",", TokenTail},
	{";", TokenHybrid},
	{"#", TokenMesh},
	{"=", TokenHalfMesh},
	{"'", TokenSpark},
	{"’", TokenSpark},
	{"`", TokenBackSpark},
	{"!", TokenWow},
	{"?", TokenWhat},
	{"\"", TokenRabbitEars},
	{"\".", TokenRabbit},
	{"\"\u0008.", TokenRabbit},
	{".\u0008\"", TokenRabbit},
	{"|", TokenSpike},
	{"%", TokenDoubleOhSeven},
	{"-", TokenWorm},
	{"<", TokenAngle},
	{">", TokenRightAngle},
	{"(", TokenWax},
	{")", TokenWane},
	{"[", TokenUTurn},
	{"]", TokenUTurnBack},
	{"{", TokenEmbrace},
	{"}", TokenBracelet},
	{"*", TokenSplat},
	{"&", TokenAmpersand},
	{"V", TokenBook},
	{"\u2228", TokenBook},
	{"V-", TokenBookworm},
	{"V\u0008-", TokenBookworm},
	{"-\u0008V", TokenBookworm},
	{"\u22bb", TokenBookworm},
	{"\u2200", TokenBookworm},
	{"$", TokenBigMoney},
	{"c/", TokenChange},
	{"c\u0008/", TokenChange},
	{"/\u0008c", TokenChange},
	{"\u00a2", TokenChange},
	{"~", TokenSqiggle},
	{"_", TokenFlatWorm},
	{"+", TokenIntersection},
	{"/", TokenSlat},
	{"\\", TokenBackSlat},
	{"@", TokenWhirlpool},
	{"'\u0008-", TokenHookworm},
	{"-\u0008'", TokenHookworm},
	{"^", TokenShark},
	{"#\u0008*\u0008[]", TokenBlotch},
	{"*\u0008#\u0008[]", TokenBlotch},
	{"#\u0008I\u0008[]", TokenBlotch},
	{"I\u0008#\u0008[]", TokenBlotch},

	{"PLEASE", TokenPlease},
	{"DO", TokenDo},
	{"NOT", TokenNot},
	{"N'T", TokenNot},
	{"CALCULATING", TokenCalculating},
	{"NEXT", TokenNext},
	{"NEXTING", TokenNexting},
	{"FORGET", TokenForget},
	{"FORGETTING", TokenForgetting},
	{"RESUME", TokenResume},
	{"RESUMING", TokenResuming},
	{"STASH", TokenStash},
	{"STASHING", TokenStashing},
	{"RETRIEVE", TokenRetrieve},
	{"RETRIEVING", TokenRetrieving},
	{"IGNORE", TokenIgnore},
	{"IGNORING", TokenIgnoring},
	{"REMEMBER", TokenRemember},
	{"REMEMBERING", TokenRemembering},
	{"ABSTAIN", TokenAbstain},
	{"ABSTAINING", TokenAbstaining},
	{"FROM", TokenFrom},
	{"REINSTATE", TokenReinstate},
	{"REINSTATING", TokenReinstating},
	{"GIVE", TokenGive},
	{"GIVING", TokenGiving},
	{"UP", TokenUp},
	{"WRITE", TokenWrite},
	{"WRITING", TokenWriting},
	{"IN", TokenIn},
	{"READ", TokenRead},
	{"READING", TokenReading},
	{"OUT", TokenOut},
	{"SUB", TokenSub},
	{"BY", TokenBy},
	{"THANK", TokenThank},
}

func init() {
	sort.Slice(tokenTable, func(i, j int) bool {
		return len(tokenTable[i].Value) > len(tokenTable[j].Value)
	})
}
