package intercal

import (
	"bytes"
	"testing"
)

func testError(t *testing.T, e error, expected string) {
	v := e.Error()
	if v != expected {
		t.Errorf("expected=%s got=%s", expected, v)
	}
}

func testErrorAt(t *testing.T, src, expected string) {
	statements, err := Parse(NewTokenizer("()", bytes.NewBufferString(src)))
	if err == nil {
		err = NewState(statements).Run(nil, nil)
	}
	if err == nil {
		t.Errorf("expected error=%s, src=%s", expected, src)
	} else {
		testError(t, err, expected)
	}
}

func TestErrors(t *testing.T) {
	testError(t, Err017, "ICL017I DO YOU EXPECT ME TO FIGURE THIS OUT?\n\tON THE WAY TO STATEMENT nnnn\n\tCORRECT SOURCE AND RESUBMIT")
	testError(t, Err123, "ICL123I PROGRAM HAS DISAPPEARED INTO THE BLACK LAGOON\n\tON THE WAY TO STATEMENT nnnn\n\tCORRECT SOURCE AND RESUBMIT")
	testErrorAt(t, "DON'T GIVE UP", "ICL633I PROGRAM FELL OFF THE EDGE\n\tON THE WAY TO STATEMENT nnnn\n\tCORRECT SOURCE AND RESUBMIT")
	testErrorAt(t, "THAT IS ADORABLE", "ICL000I THAT IS A\n\tON THE WAY TO STATEMENT 0002\n\tCORRECT SOURCE AND RESUBMIT")
	testErrorAt(t, "DON'T GO AND DON'T COME BACK AND (1) DO FORGET #1 DON'T FORGET TO PLEASE DO (1) NEXT", "ICL774I RANDOM COMPILER BUG\n\tON THE WAY TO STATEMENT 0003\n\tCORRECT SOURCE AND RESUBMIT")
	testErrorAt(t, "(1) PLEASE NOTE STACK OVERFLOW DO FORGET #0 DO (2) NEXT DO (3) NEXT THANK YOU (2) DO (1) NEXT", "ICL123I PROGRAM HAS DISAPPEARED INTO THE BLACK LAGOON\n\tON THE WAY TO STATEMENT 0002\n\tCORRECT SOURCE AND RESUBMIT")
}
