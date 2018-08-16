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
	testError(t, Err017, "ICL017I (An expression contains a syntax error.)")
	testError(t, Err123, "ICL123I (Program has attempted 80 levels of NEXTing.)\nPROGRAM HAS DISAPPEARED INTO THE BLACK LAGOON.\n")
	testError(t, Err017.At(0), "ICL017I (An expression contains a syntax error.)\n        ON THE WAY TO STATEMENT 0001\n        CORRECT SOURCE AND RESUBMIT\n")
	testError(t, Err123.At(0), "ICL123I (Program has attempted 80 levels of NEXTing.)\n        ON THE WAY TO STATEMENT 0001\n        CORRECT SOURCE AND RESUBMIT\nPROGRAM HAS DISAPPEARED INTO THE BLACK LAGOON.\n")
}
