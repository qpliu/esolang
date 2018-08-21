package main

import (
	"testing"
)

func testError(t *testing.T, e *Error, expected string) {
	v := e.Error()
	if v != expected {
		t.Errorf("expected=%s got=%s", expected, v)
	}
}

func TestErrors(t *testing.T) {
	testError(t, Err017, "ICL017I DO YOU EXPECT ME TO FIGURE THIS OUT?")
	testError(t, Err123, "ICL123I PROGRAM HAS DISAPPEARED INTO THE BLACK LAGOON")
	testError(t, Err017.At(0), "ICL017I DO YOU EXPECT ME TO FIGURE THIS OUT?\n        ON THE WAY TO STATEMENT 0001\n        CORRECT SOURCE AND RESUBMIT\n")
	testError(t, Err123.At(0), "ICL123I PROGRAM HAS DISAPPEARED INTO THE BLACK LAGOON\n        ON THE WAY TO STATEMENT 0001\n        CORRECT SOURCE AND RESUBMIT\n")
}
