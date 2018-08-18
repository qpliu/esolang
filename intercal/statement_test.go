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
    2    (5) DO FORGET #1
    3        PLEASE WRITE IN :1
    4        DO .1 <- '⊻":1~'#32768¢#0'"¢#1'~#3
    5        DO (1) NEXT
    6        DO :1 <- "'⊻":1~'#65535¢#0'"¢#65535'
       ~'#0¢#65535'"¢"'⊻":1~'#0¢#65535'"
       ¢#65535'~'#0¢#65535'"
    7        DO :2 <- #1
    8        PLEASE DO (4) NEXT
    9    (4) DO FORGET #1
   10        DO .1 <- "⊻’:1~:2’¢#1"~#3
   11        DO :1 <- "'⊻":1~'#65535¢#0'"¢":2~'#65535
       ¢#0'"'~'#0¢#65535'"¢"'⊻":1~'#0
       ¢#65535'"¢":2~'#0¢#65535'"'~'#0¢#65535'"
   12        DO (1) NEXT
   13        DO :2 <- ":2~'#0¢#65535'"
       ¢"'":2~'#65535¢#0'"¢#0'~'#32767¢#1'"
   14        DO (4) NEXT
   15    (2) DO RESUME .1
   16    (1) PLEASE DO (2) NEXT
   17        PLEASE FORGET #1
   18        DO READ OUT :1
   19        PLEASE DO .1 <- '⊻"':1~:1'~#1"¢#1'~#3
   20        DO (3) NEXT
   21        PLEASE DO (5) NEXT
   22    (3) DO (2) NEXT
   23        PLEASE GIVE UP
`, "")
}

func testParseStatements(t *testing.T, src string, expected []StatementType) []*Statement {
	statements, err := Parse(NewTokenizer("()", bytes.NewBufferString(src)))
	if err != nil {
		t.Errorf("unexpected parse error=%s", err.Error())
	}
	if len(statements) != len(expected) {
		t.Errorf("len(statements) expected=%d got=%d", len(expected), len(statements))
	} else {
		for i, s := range statements {
			if s.Type != expected[i] {
				t.Errorf("statement[%d] expected=%d got=%d %s", i, expected[i], s.Type, s.String())
			}
		}
	}
	return statements
}

func TestParseStatements(t *testing.T) {
	testParseStatements(t, "DO ,1SUB#1#1 <- ’⊻,1SUB#1#1¢#1’~’#0¢#65535’", []StatementType{StatementCalculate})
	testParseStatements(t, `DO ;7 <- #7 BY .7 BY ",7SUB#7"¢’:7~#7’`, []StatementType{StatementCalculateArrayDimension})
	testParseStatements(t, `DO ;7 <- #7 BY .7 BY ,7SUB"#7¢’:7~#7’"`, []StatementType{StatementUnrecognizable}) // because 32-bit array subscripts are not allowed
	testParseStatements(t, "DO (1) NEXT (1) DO GIVE UP", []StatementType{StatementNext, StatementGiveUp})
	testParseStatements(t, "PLEASE DO (1) NEXT (1) DO FORGET #1", []StatementType{StatementNext, StatementForget})
	testParseStatements(t, "PLEASE (1) NEXT (1) DO %1000 RESUME #1", []StatementType{StatementNext, StatementResume})
	testParseStatements(t, "PLEASE STASH .123+:123+,123", []StatementType{StatementStash})
	testParseStatements(t, "PLEASE DO RETRIEVE .123+:123+,123", []StatementType{StatementRetrieve})
	testParseStatements(t, "PLEASE IGNORE .1", []StatementType{StatementIgnore})
	testParseStatements(t, "PLEASE REMEMBER ;1", []StatementType{StatementRemember})
	testParseStatements(t, "PLEASE ABSTAIN FROM STASHING", []StatementType{StatementAbstainGerundList})
	testParseStatements(t, "PLEASE ABSTAIN FROM IGNORING + FORGETTING", []StatementType{StatementAbstainGerundList})
	testParseStatements(t, "PLEASE ABSTAIN FROM NEXTING", []StatementType{StatementAbstainGerundList})
	testParseStatements(t, "PLEASE ABSTAIN FROM CALCULATING", []StatementType{StatementAbstainGerundList})
	testParseStatements(t, "DO ABSTAIN FROM ABSTAINING", []StatementType{StatementAbstainGerundList})
	testParseStatements(t, "DO ABSTAIN FROM REINSTATING", []StatementType{StatementAbstainGerundList})
	testParseStatements(t, "DO ABSTAIN FROM GIVING UP", []StatementType{StatementUnrecognizable}) // not accepted
	testParseStatements(t, "DON'T GIVE UP", []StatementType{StatementGiveUp})
	testParseStatements(t, "(1) DO ABSTAIN FROM (1)", []StatementType{StatementAbstainLabel})
	testParseStatements(t, "(1) DO REINSTATE (1)", []StatementType{StatementReinstateLabel})
	testParseStatements(t, "PLEASE REINSTATE REINSTATING", []StatementType{StatementReinstateGerundList})
	testParseStatements(t, "DO REINSTATE GIVING UP", []StatementType{StatementUnrecognizable}) // invalid
	testParseStatements(t, "PLEASE REINSTATE (1) (1) DON'T GIVE UP", []StatementType{StatementReinstateLabel, StatementGiveUp})
	testParseStatements(t, "PLEASE GIVE UP", []StatementType{StatementGiveUp})
	testParseStatements(t, "PLEASE RESUME #80", []StatementType{StatementResume})
	testParseStatements(t, "DO WRITE IN .1 + :1 + ,1 + ,1 SUB #1!1' + ;1", []StatementType{StatementWriteIn})
	testParseStatements(t, "DO READ OUT .1 + :1 + ,1 SUB #1 + ;1 SUB #1 #1 + #1", []StatementType{StatementReadOut})
	testParseStatements(t, "PLEASE NOTE THIS LINE HAS NO EFFECT", []StatementType{StatementUnrecognizable})
	testParseStatements(t, "DO SOMETHING ABOUT OVERFLOW IN ;3", []StatementType{StatementUnrecognizable})
	testParseStatements(t, "(123) DON'T YOU REALIZE THIS STATEMENT SHOULD ONLY BE ENCOUNTERED ONCE? PLEASE REINSTATE (123)", []StatementType{StatementUnrecognizable, StatementReinstateLabel})
	testParseStatements(t, "PLEASE NOTE THAT THIS LINE DOES NOTHING", []StatementType{StatementUnrecognizable, StatementUnrecognizable})

	stmts := testParseStatements(t, "DO .1 <- #&255", []StatementType{StatementCalculate})
	if _, ok := stmts[0].Operands.(Calculation); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO ,1 <- #&255", []StatementType{StatementCalculateArrayDimension})
	if _, ok := stmts[0].Operands.(Dimensioning); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "(1) DO (1) NEXT", []StatementType{StatementNext})
	if _, ok := stmts[0].Operands.(int); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "PLEASE FORGET #1", []StatementType{StatementForget})
	if _, ok := stmts[0].Operands.(Expr); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "PLEASE DO RESUME ,1SUB#1", []StatementType{StatementResume})
	if _, ok := stmts[0].Operands.(Expr); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "PLEASE STASH .1", []StatementType{StatementStash})
	if _, ok := stmts[0].Operands.([]Stashable); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO RETRIEVE :1+,1+;1", []StatementType{StatementRetrieve})
	if _, ok := stmts[0].Operands.([]Stashable); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO IGNORE .1+.2+.3+.4", []StatementType{StatementIgnore})
	if _, ok := stmts[0].Operands.([]Stashable); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO REMEMBER .1+.1+.1+.1", []StatementType{StatementRemember})
	if _, ok := stmts[0].Operands.([]Stashable); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "(1) DON'T ABSTAIN FROM (1)", []StatementType{StatementAbstainLabel})
	if _, ok := stmts[0].Operands.(int); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO NOT ABSTAIN FROM ABSTAINING", []StatementType{StatementAbstainGerundList})
	if _, ok := stmts[0].Operands.([]int); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "(1) DO REINSTATE (1)", []StatementType{StatementReinstateLabel})
	if _, ok := stmts[0].Operands.(int); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO REINSTATE REINSTATING + CALCULATING", []StatementType{StatementReinstateGerundList})
	if _, ok := stmts[0].Operands.([]int); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO NOT GIVE UP", []StatementType{StatementGiveUp})
	if stmts[0].Operands != nil {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO WRITE IN ,1 SUB .2 + ,3 + ;4 SUB #5 + :6", []StatementType{StatementWriteIn})
	if _, ok := stmts[0].Operands.([]WriteInable); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO READ OUT ,1 SUB .2 + #3 + ;4 SUB #5 + :6", []StatementType{StatementReadOut})
	if _, ok := stmts[0].Operands.([]ReadOutable); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}
}

func testParseExpr(t *testing.T, src string, expected Expr) {
	stmts := testParseStatements(t, "PLEASE DON'T FORGET"+src, []StatementType{StatementForget})
	if expected != stmts[0].Operands {
		t.Errorf("parse expr failed: %s %v", src, stmts[0].Error)
	}
}

func testParseBadExpr(t *testing.T, src string) {
	stmts := testParseStatements(t, "PLEASE DON'T FORGET"+src, []StatementType{StatementUnrecognizable})
	if stmts[0].Error != Err017 {
		t.Errorf("parse bad expr: %s %v", src, stmts[0].Error)
	}
}

func TestParseExpr(t *testing.T) {
	testParseExpr(t, ".123", Var16(123))
	testParseExpr(t, ":123", Var32(123))
	testParseExpr(t, ".1", Var16(1))
	testParseExpr(t, ".001", Var16(1))
	testParseExpr(t, "#1", ExprConst(1))

	testParseExpr(t, "#65535$#0", ExprMingle{ExprConst(65535), ExprConst(0)})
	testParseExpr(t, "#0$#65535", ExprMingle{ExprConst(0), ExprConst(65535)})
	testParseExpr(t, "#255$#255", ExprMingle{ExprConst(255), ExprConst(255)})
	testParseExpr(t, "#65535", ExprConst(65535))
	testParseExpr(t, "#179~#201", ExprSelect{ExprConst(179), ExprConst(201)})
	testParseExpr(t, "#201~#179", ExprSelect{ExprConst(201), ExprConst(179)})
	testParseExpr(t, "#179~#179", ExprSelect{ExprConst(179), ExprConst(179)})
	testParseExpr(t, "#201~#201", ExprSelect{ExprConst(201), ExprConst(201)})

	testParseExpr(t, ".&123", ExprAnd{Var16(123)})
	testParseExpr(t, "'.&123'", ExprAnd{Var16(123)})
	testParseExpr(t, "!&123'", ExprAnd{Var16(123)})
	testParseExpr(t, "'&.123'", ExprAnd{Var16(123)})
	testParseExpr(t, "\"&.123\"", ExprAnd{Var16(123)})
	testParseExpr(t, "#V123", ExprOr{ExprConst(123)})
	testParseExpr(t, "#&77", ExprAnd{ExprConst(77)})
	testParseExpr(t, "#V77", ExprOr{ExprConst(77)})
	testParseExpr(t, "#?77", ExprXor{ExprConst(77)})
	testParseBadExpr(t, "#V&123")

	testParseExpr(t, "'#165$#203'~#358", ExprSelect{ExprMingle{ExprConst(165), ExprConst(203)}, ExprConst(358)})
	testParseExpr(t, "#165$'#203~#358'", ExprMingle{ExprConst(165), ExprSelect{ExprConst(203), ExprConst(358)}})
	// testParseBadExpr(t, "#165$#203~#358")
	testParseExpr(t, "'V#&123'", ExprOr{ExprAnd{ExprConst(123)}})
	testParseExpr(t, "'V\"&#123\"'", ExprOr{ExprAnd{ExprConst(123)}})
	testParseExpr(t, "'V'&#123''", ExprOr{ExprAnd{ExprConst(123)}})
	testParseExpr(t, "'.1~.2'", ExprSelect{Var16(1), Var16(2)})
	testParseExpr(t, "!1~.2'", ExprSelect{Var16(1), Var16(2)})
	testParseExpr(t, "'V.1$.2'", ExprOr{ExprMingle{Var16(1), Var16(2)}})
	testParseExpr(t, "\"V!1$.2'\"", ExprOr{ExprMingle{Var16(1), Var16(2)}})
	testParseExpr(t, "\"V.1$!2'\"", ExprOr{ExprMingle{Var16(1), Var16(2)}})
	// testParseExpr(t, "'V.1$!2''", ExprOr{ExprMingle{Var16(1), Var16(2)}})

	// ",1 SUB #1 ~ #2"
	// ",1 SUB ,2 SUB #1 #2 #3"
	// ",1 SUB ' ,2 SUB ' ,3 SUB #1 ' #2 ' ' #3 '"
}
