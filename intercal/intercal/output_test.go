package intercal

import (
	"bytes"
	"testing"
)

func testOutput(t *testing.T, value uint32, expected string) {
	var b bytes.Buffer
	Output(&b, value)
	out := b.String()
	if out != expected+"\n" {
		t.Errorf("for %d, expected=%s, got %s", value, expected, out)
	}
}

func TestOutput(t *testing.T) {
	testOutput(t, 0, "\u203e")
	testOutput(t, 1, "I")
	testOutput(t, 2, "II")
	testOutput(t, 3, "III")
	testOutput(t, 4, "IV")
	testOutput(t, 5, "V")
	testOutput(t, 6, "VI")
	testOutput(t, 7, "VII")
	testOutput(t, 8, "VIII")
	testOutput(t, 9, "IX")
	testOutput(t, 10, "X")
	testOutput(t, 16, "XVI")
	testOutput(t, 32, "XXXII")
	testOutput(t, 49, "IL")
	testOutput(t, 64, "LXIV")
	testOutput(t, 99, "IC")
	testOutput(t, 128, "CXXVIII")
	testOutput(t, 256, "CCLVI")
	testOutput(t, 498, "XDVIII")
	testOutput(t, 499, "ID")
	testOutput(t, 512, "DXII")
	testOutput(t, 599, "DIC")
	testOutput(t, 1024, "MXXIV")
	testOutput(t, 2048, "MMXLVIII")
	testOutput(t, 3998, "MMMXMVIII")
	testOutput(t, 3999, "MMMIM")
	testOutput(t, 4000, "I\u0305V\u0305")
	testOutput(t, 4096, "I\u0305V\u0305XCVI")
	testOutput(t, 8192, "V\u0305I\u0305I\u0305I\u0305CXCII")
	testOutput(t, 16384, "X\u0305V\u0305I\u0305CCCLXXXIV")
	testOutput(t, 32768, "X\u0305X\u0305X\u0305I\u0305I\u0305DCCLXVIII")
	testOutput(t, 65536, "L\u0305X\u0305V\u0305DXXXVI")
	testOutput(t, 131072, "C\u0305X\u0305X\u0305X\u0305I\u0305LXXII")
	testOutput(t, 262144, "C\u0305C\u0305L\u0305X\u0305I\u0305I\u0305CXLIV")
	testOutput(t, 524288, "D\u0305X\u0305X\u0305I\u0305V\u0305CCLXXXVIII")
	testOutput(t, 1048576, "M\u0305X\u0305L\u0305V\u0305I\u0305I\u0305I\u0305DLXXVI")
	testOutput(t, 2097152, "M\u0305M\u0305X\u0305C\u0305V\u0305I\u0305I\u0305CLII")
	testOutput(t, 4194304, "ivC\u0305X\u0305C\u0305I\u0305V\u0305CCCIV")
	testOutput(t, 8388608, "viiiC\u0305C\u0305C\u0305L\u0305X\u0305X\u0305X\u0305V\u0305I\u0305I\u0305I\u0305DCVIII")
	testOutput(t, 16777216, "xviD\u0305C\u0305C\u0305L\u0305X\u0305X\u0305V\u0305I\u0305I\u0305CCXVI")
	testOutput(t, 33554432, "xxxiiiD\u0305L\u0305I\u0305V\u0305CDXXXII")
	testOutput(t, 67108864, "lxviiC\u0305V\u0305I\u0305I\u0305I\u0305DCCCLXIV")
	testOutput(t, 134217728, "cxxxivC\u0305C\u0305X\u0305V\u0305I\u0305I\u0305DCCXXVIII")
	testOutput(t, 268435456, "cclxviiiC\u0305D\u0305X\u0305X\u0305X\u0305V\u0305CDLVI")
	testOutput(t, 536870912, "dxxxviD\u0305C\u0305C\u0305C\u0305L\u0305X\u0305X\u0305CMXII")
	testOutput(t, 1073741824, "mlxxiiiD\u0305C\u0305C\u0305X\u0305L\u0305I\u0305DCCCXXIV")
	testOutput(t, 2147483648, "mmcxlviiC\u0305D\u0305L\u0305X\u0305X\u0305X\u0305I\u0305I\u0305I\u0305DCXLVIII")
	testOutput(t, 4294967295, "i\u0305v\u0305ccxcivC\u0305M\u0305L\u0305X\u0305V\u0305I\u0305I\u0305CCXCV")
}
