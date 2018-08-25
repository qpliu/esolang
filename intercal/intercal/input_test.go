package intercal

import (
	"bytes"
	"testing"
)

func testInput16(t *testing.T, input string, values []uint16, endErr error) {
	r := NewIntercalReader(bytes.NewBufferString(input))
	for _, v := range values {
		value, err := r.Input16()
		if err != nil {
			t.Errorf("expected %d, got error=%s", v, err.Error())
		} else if v != value {
			t.Errorf("expected %d, got %d", v, value)
		}
	}
	value, err := r.Input16()
	if err == nil {
		t.Errorf("expected error=%s, got %d", endErr.Error(), value)
	} else if err.Error() != endErr.Error() {
		t.Errorf("expected error=%s, got error=%s", endErr.Error(), err.Error())
	}
}

func testInput32(t *testing.T, input string, values []uint32, endErr error) {
	r := NewIntercalReader(bytes.NewBufferString(input))
	for _, v := range values {
		value, err := r.Input32()
		if err != nil {
			t.Errorf("expected %d, got error=%s", v, err.Error())
		} else if v != value {
			t.Errorf("expected %d, got %d", v, value)
		}
	}
	value, err := r.Input32()
	if err == nil {
		t.Errorf("expected error=%s, got %d", endErr.Error(), value)
	} else if err.Error() != endErr.Error() {
		t.Errorf("expected error=%s, got error=%s", endErr.Error(), err.Error())
	}
}

func TestInput(t *testing.T) {
	testInput16(t, "OH\nZERO\nONE\nTWO\nTHREE\nFOUR\nFIVE\nSIX\nSEVEN\nEIGHT\nNINE\nONE FIVE\nTWO FIVE FIVE\nTHREE TWO SEVEN SIX SEVEN\n", []uint16{0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 15, 255, 32767}, Err562)
	testInput16(t, "OCHO CINCO\n", []uint16{}, Err579.WithMessage("WHAT BASE AND/OR LANGUAGE INCLUDES OCHO?"))
	testInput16(t, "\n", []uint16{}, Err562)
	testInput16(t, "", []uint16{}, Err562)
	testInput16(t, "ONE \n", []uint16{1}, Err562)
	testInput16(t, " SIX FIVE  FIVE THREE SIX \n", []uint16{}, Err275)
	testInput32(t, " SIX FIVE  FIVE THREE SIX \n", []uint32{65536}, Err562)
	testInput32(t, " FOUR  TWO NINE FOUR  NINE SIX SEVEN  TWO NINE SIX \n", []uint32{}, Err533)
}
