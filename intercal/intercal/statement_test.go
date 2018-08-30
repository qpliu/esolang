package intercal

import (
	"bytes"
	"testing"
)

func testParseList(t *testing.T, src, expectedListing string, errorCode int) {
	statements, err := Parse(NewTokenizer("()", bytes.NewBufferString(src)))
	if err != nil {
		if errorCode == -1 {
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
`, `*0001 PLEASE NOTE THIS LINE HAS NO EFFECT
`, 0)
	testParseList(t, `PLEASE NOTE THIS LINE DOES NOTHING
`, `*0001 PLEASE NOTE THIS LINE 
*0002 DOES NOTHING
`, -1)
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
`, ` 0001         DO (5) NEXT
 0002    (5) DO FORGET #1
 0003        PLEASE WRITE IN :1
 0004        DO .1 <- '⊻":1~'#32768¢#0'"¢#1'~#3
 0005        DO (1) NEXT
 0006        DO :1 <- "'⊻":1~'#65535¢#0'"¢#65535'
       ~'#0¢#65535'"¢"'⊻":1~'#0¢#65535'"
       ¢#65535'~'#0¢#65535'"
 0007        DO :2 <- #1
 0008        PLEASE DO (4) NEXT
 0009    (4) DO FORGET #1
 0010        DO .1 <- "⊻’:1~:2’¢#1"~#3
 0011        DO :1 <- "'⊻":1~'#65535¢#0'"¢":2~'#65535
       ¢#0'"'~'#0¢#65535'"¢"'⊻":1~'#0
       ¢#65535'"¢":2~'#0¢#65535'"'~'#0¢#65535'"
 0012        DO (1) NEXT
 0013        DO :2 <- ":2~'#0¢#65535'"
       ¢"'":2~'#65535¢#0'"¢#0'~'#32767¢#1'"
 0014        DO (4) NEXT
 0015    (2) DO RESUME .1
 0016    (1) PLEASE DO (2) NEXT
 0017        PLEASE FORGET #1
 0018        DO READ OUT :1
 0019        PLEASE DO .1 <- '⊻"':1~:1'~#1"¢#1'~#3
 0020        DO (3) NEXT
 0021        PLEASE DO (5) NEXT
 0022    (3) DO (2) NEXT
 0023        PLEASE GIVE UP
`, -1)
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
	if _, ok := stmts[0].Operands.([]int); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO NOT ABSTAIN FROM ABSTAINING", []StatementType{StatementAbstainGerundList})
	if _, ok := stmts[0].Operands.([]int); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "(1) DO REINSTATE (1)", []StatementType{StatementReinstateLabel})
	if _, ok := stmts[0].Operands.([]int); !ok {
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

	stmts = testParseStatements(t, "DO READ OUT", []StatementType{StatementReadOutBit})
	if _, ok := stmts[0].Operands.(bool); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO READ NAUGHT", []StatementType{StatementReadOutBit})
	if _, ok := stmts[0].Operands.(bool); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO READ NOT", []StatementType{StatementReadOutBit})
	if _, ok := stmts[0].Operands.(bool); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}

	stmts = testParseStatements(t, "DO READN'T", []StatementType{StatementReadOutBit})
	if _, ok := stmts[0].Operands.(bool); !ok {
		t.Errorf("unexpected operand: %s", stmts[0].String())
	}
}

func exprEqual(expr1, expr2 Expr) bool {
	switch e1 := expr1.(type) {
	case ArrayElement:
		e2, ok := expr2.(ArrayElement)
		if !ok || e1.Array != e2.Array || len(e1.Index) != len(e2.Index) {
			return false
		}
		for i := range e1.Index {
			if !exprEqual(e1.Index[i], e2.Index[i]) {
				return false
			}
		}
		return true
	case ExprMingle:
		e2, ok := expr2.(ExprMingle)
		return ok && exprEqual(e1[0], e2[0]) && exprEqual(e1[1], e2[1])
	case ExprSelect:
		e2, ok := expr2.(ExprSelect)
		return ok && exprEqual(e1[0], e2[0]) && exprEqual(e1[1], e2[1])
	case ExprAnd:
		e2, ok := expr2.(ExprAnd)
		return ok && exprEqual(e1[0], e2[0])
	case ExprOr:
		e2, ok := expr2.(ExprOr)
		return ok && exprEqual(e1[0], e2[0])
	case ExprXor:
		e2, ok := expr2.(ExprXor)
		return ok && exprEqual(e1[0], e2[0])
	default:
		return expr1 == expr2
	}
}

func testParseExpr(t *testing.T, src string, expected Expr) {
	stmts := testParseStatements(t, "PLEASE DON'T FORGET"+src, []StatementType{StatementForget})
	if !exprEqual(stmts[0].Operands.(Expr), expected) {
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
	testParseBadExpr(t, "#165$#203~#358")
	testParseExpr(t, "'V#&123'", ExprOr{ExprAnd{ExprConst(123)}})
	testParseExpr(t, "'V\"&#123\"'", ExprOr{ExprAnd{ExprConst(123)}})
	testParseExpr(t, "'V'&#123''", ExprOr{ExprAnd{ExprConst(123)}})
	testParseExpr(t, "'.1~.2'", ExprSelect{Var16(1), Var16(2)})
	testParseExpr(t, "!1~.2'", ExprSelect{Var16(1), Var16(2)})
	testParseExpr(t, "'V.1$.2'", ExprOr{ExprMingle{Var16(1), Var16(2)}})
	testParseExpr(t, "\"V!1$.2'\"", ExprOr{ExprMingle{Var16(1), Var16(2)}})
	testParseExpr(t, "\"V.1$!2'\"", ExprOr{ExprMingle{Var16(1), Var16(2)}})
	testParseExpr(t, "'V.1$!2''", ExprOr{ExprMingle{Var16(1), Var16(2)}})

	testParseExpr(t, ",1 SUB #1 ~ #2", ArrayElement{Array16(1), []Expr{ExprSelect{ExprConst(1), ExprConst(2)}}})
	testParseExpr(t, ",1 SUB ,2 SUB #1 #2 #3", ArrayElement{Array16(1), []Expr{ArrayElement{Array16(2), []Expr{ExprConst(1), ExprConst(2), ExprConst(3)}}}})
	testParseExpr(t, ",1 SUB ' ,2 SUB ' ,3 SUB #1 ' #2 ' ' #3 '", ArrayElement{Array16(1), []Expr{ArrayElement{Array16(2), []Expr{ArrayElement{Array16(3), []Expr{ExprConst(1)}}, ExprConst(2)}}, ExprConst(3)}})

	testParseExpr(t, "’⊻,1SUB#1#1¢#1’~’#0¢#65535’", ExprSelect{ExprXor{ExprMingle{ArrayElement{Array16(1), []Expr{ExprConst(1), ExprConst(1)}}, ExprConst(1)}}, ExprMingle{ExprConst(0), ExprConst(65535)}})
	testParseExpr(t, "'?\",1SUB#1#1\"$#1'~'#0$#65535'", ExprSelect{ExprXor{ExprMingle{ArrayElement{Array16(1), []Expr{ExprConst(1), ExprConst(1)}}, ExprConst(1)}}, ExprMingle{ExprConst(0), ExprConst(65535)}})
	testParseExpr(t, "’∀,1SUB#1#1¢#1’~’#0¢#65535’", ExprSelect{ExprXor{ExprMingle{ArrayElement{Array16(1), []Expr{ExprConst(1), ExprConst(1)}}, ExprConst(1)}}, ExprMingle{ExprConst(0), ExprConst(65535)}})

	testParseExpr(t, "\",1SUB\",2SUB#1\"#2\"", ArrayElement{Array16(1), []Expr{ArrayElement{Array16(2), []Expr{ExprConst(1)}}, ExprConst(2)}})
	testParseExpr(t, "\",1SUB',2SUB#1'#2\"", ArrayElement{Array16(1), []Expr{ArrayElement{Array16(2), []Expr{ExprConst(1)}}, ExprConst(2)}})

	testParseExpr(t, "\"?'\"!1~.1'~#1\"$#1'\"~#3", ExprSelect{ExprXor{ExprMingle{ExprSelect{ExprSelect{Var16(1), Var16(1)}, ExprConst(1)}, ExprConst(1)}}, ExprConst(3)})

	testParseExpr(t, "',2SUB.1\".2~.3\"'~#1", ExprSelect{ArrayElement{Array16(2), []Expr{Var16(1), ExprSelect{Var16(2), Var16(3)}}}, ExprConst(1)})
	testParseExpr(t, ",3SUB\",2SUB.1'.2~.3'\".4", ArrayElement{Array16(3), []Expr{ArrayElement{Array16(2), []Expr{Var16(1), ExprSelect{Var16(2), Var16(3)}}}, Var16(4)}})
}
