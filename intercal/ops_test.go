package main

import (
	"testing"
)

func testResult(t *testing.T, msg string, actual, expected uint32) {
	if actual != expected {
		t.Errorf("%s, got=%d, expected=%d", msg, actual, expected)
	}
}

func TestOpMingle(t *testing.T) {
	testResult(t, "#65535¢#0", OpMingle(65535, 0), 2863311530)
	testResult(t, "#0¢#65535", OpMingle(0, 65535), 1431655765)
	testResult(t, "#255¢#255", OpMingle(255, 255), 65535)
}

func TestOpSelect(t *testing.T) {
	testResult(t, "#179~#201", OpSelect(179, 201), 9)
	testResult(t, "#201~#179", OpSelect(201, 179), 17)
	testResult(t, "#179~#179", OpSelect(179, 179), 31)
	testResult(t, "#201~#201", OpSelect(201, 201), 15)
}

func TestOpAnd(t *testing.T) {
	testResult(t, "#&77", OpAnd16(77), 4)
	testResult(t, "\"&#77~'#65535¢#65535'\"", OpAnd32(77), 4)
}

func TestOpOr(t *testing.T) {
	testResult(t, "#V77", OpOr16(77), 32879)
	testResult(t, "\"V#77~'#65535¢#65535'\"", OpOr32(77), 2147483759)
}

func TestOpXor(t *testing.T) {
	testResult(t, "#V77", OpXor16(77), 32875)
	testResult(t, "\"V#77~'#65535¢#65535'\"", OpXor32(77), 2147483755)
}

func TestOps(t *testing.T) {
	testResult(t, "'#165¢#203'~#358", OpSelect(OpMingle(165, 203), 358), 15)
	testResult(t, "#165¢'#203~#358'", OpMingle(165, OpSelect(203, 358)), 34915)
}
