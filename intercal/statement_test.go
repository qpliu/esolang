package main

import (
	"bytes"
	"testing"
)

func testParseList(t *testing.T, src, expectedListing string, errorCode string) {
	statements, err := Parse(NewTokenizer("()", bytes.NewBufferString(src)))
	if err != nil {
		if errorCode == "" {
			t.Errorf("unexpected error=%s", err.Error())
			return
		} else if e, ok := err.(*Error); !ok {
			t.Errorf("unexpected error=%s", err.Error())
			return
		} else if e.Code() != errorCode {
			t.Errorf("unexpected error=%s", err.Error())
			return
		}
	}
	b := bytes.Buffer{}
	err = ListStatements(statements, &b)
	if err != nil {
		t.Errorf("unexpected error=%s", err.Error())
		return
	}
	listing := b.String()
	if listing != expectedListing {
		t.Errorf("listing does not match expected, listing=%s\nexpected=%s\n", listing, expectedListing)
	}
}

func TestParseList(t *testing.T) {
	testParseList(t, `PLEASE NOTE THIS LINE HAS NO EFFECT
`, `*   1 PLEASE NOTE THIS LINE HAS NO EFFECT
`, "")
	testParseList(t, `PLEASE NOTE THIS LINE DOES NOTHING
`, `*   1 PLEASE NOTE THIS LINE 
*   2 DOES NOTHING
`, "")
	testParseList(t, `        DO (5) NEXT
    (5) DO FORGET #1
        PLEASE WRITE IN :1
        DO .1 <- '⊻":1~'#32768¢#0'"¢#1'~#3
        DO (1) NEXT
        DO :1 <- "'⊻":1~'#65535¢#0'"¢#65535'
        ~'#0¢#65535'"¢"'⊻":1~'#0¢#65535'"
        ¢#65535'~'#0¢#65535'"
        DO :2 <- #1
        PLEASE DO (4) NEXT
    (4) DO FORGET #1
        DO .1 <- "⊻’:1~:2’¢#1"~#3
        DO :1 <- "'⊻":1~'#65535¢#0'"¢":2~'#65535
        ¢#0'"'~'#0¢#65535'"¢"'⊻":1~'#0
        ¢#65535'"¢":2~'#0¢#65535'"'~'#0¢#65535'"
        DO (1) NEXT
        DO :2 <- ":2~'#0¢#65535'"
        ¢"'":2~'#65535¢#0'"¢#0'~'#32767¢#1'"
        DO (4) NEXT
    (2) DO RESUME .1
    (1) PLEASE DO (2) NEXT
        PLEASE FORGET #1
        DO READ OUT :1
        PLEASE DO .1 <- '⊻"':1~:1'~#1"¢#1'~#3
        DO (3) NEXT
        PLEASE DO (5) NEXT
    (3) DO (2) NEXT
        PLEASE GIVE UP
`, `    1         DO (5) NEXT
*   2     (5) DO FORGET #1
*   3         PLEASE WRITE IN :1
*   4         DO .1 <- '⊻":1~'#32768¢#0'"¢#1'~#3
    5         DO (1) NEXT
*   6         DO :1 <- "'⊻":1~'#65535¢#0'"¢#65535'
        ~'#0¢#65535'"¢"'⊻":1~'#0¢#65535'"
        ¢#65535'~'#0¢#65535'"
*   7         DO :2 <- #1
    8         PLEASE DO (4) NEXT
*   9     (4) DO FORGET #1
*  10         DO .1 <- "⊻’:1~:2’¢#1"~#3
*  11         DO :1 <- "'⊻":1~'#65535¢#0'"¢":2~'#65535
        ¢#0'"'~'#0¢#65535'"¢"'⊻":1~'#0
        ¢#65535'"¢":2~'#0¢#65535'"'~'#0¢#65535'"
   12         DO (1) NEXT
*  13         DO :2 <- ":2~'#0¢#65535'"
        ¢"'":2~'#65535¢#0'"¢#0'~'#32767¢#1'"
   14         DO (4) NEXT
*  15     (2) DO RESUME .1
   16     (1) PLEASE DO (2) NEXT
*  17         PLEASE FORGET #1
*  18         DO READ OUT :1
*  19         PLEASE DO .1 <- '⊻"':1~:1'~#1"¢#1'~#3
   20         DO (3) NEXT
   21         PLEASE DO (5) NEXT
   22     (3) DO (2) NEXT
   23         PLEASE GIVE UP
`, "")
}
