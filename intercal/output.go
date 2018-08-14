package main

import (
	"io"
)

const (
	outputDigitsL = "ivxlcdm"
	outputDigitsU = "IVXLCDM"

	outputI = 0
	outputV = 1
	outputX = 2
	outputL = 3
	outputC = 4
	outputD = 5
	outputM = 6

	zeroOverline = "\u203e\n"
	noOverline   = ""
	addOverline  = "\u0305"
)

func Output(w io.Writer, value uint32) {
	if value == 0 {
		w.Write([]byte(zeroOverline))
		return
	}
	if value >= 4000000000 {
		output(w, value/1000000000, outputDigitsL, addOverline)
		value %= 1000000000
	}
	if value >= 4000000 {
		output(w, value/1000000, outputDigitsL, noOverline)
		value %= 1000000
	}
	if value >= 4000 {
		output(w, value/1000, outputDigitsU, addOverline)
		value %= 1000
	}
	output(w, value, outputDigitsU, noOverline)
	w.Write([]byte("\n"))
}

func output(w io.Writer, value uint32, digits string, overline string) {
	if value == 0 {
		return
	}
	outputTens(w, digits, overline, outputM, value/1000)
	if value > 1000 && value%1000 == 999 {
		outputDigit(w, digits, overline, outputI)
		outputDigit(w, digits, overline, outputM)
		return
	}
	outputTens(w, digits, overline, outputC, (value/100)%10)
	outputTens(w, digits, overline, outputX, (value/10)%10)
	outputTens(w, digits, overline, outputI, value%10)
}

func outputTens(w io.Writer, digits string, overline string, index int, value uint32) {
	switch value {
	case 9:
		outputDigit(w, digits, overline, index)
		outputDigit(w, digits, overline, index+2)
	case 8:
		outputDigit(w, digits, overline, index+1)
		outputDigit(w, digits, overline, index)
		outputDigit(w, digits, overline, index)
		outputDigit(w, digits, overline, index)
	case 7:
		outputDigit(w, digits, overline, index+1)
		outputDigit(w, digits, overline, index)
		outputDigit(w, digits, overline, index)
	case 6:
		outputDigit(w, digits, overline, index+1)
		outputDigit(w, digits, overline, index)
	case 5:
		outputDigit(w, digits, overline, index+1)
	case 4:
		outputDigit(w, digits, overline, index)
		outputDigit(w, digits, overline, index+1)
	case 3:
		outputDigit(w, digits, overline, index)
		outputDigit(w, digits, overline, index)
		outputDigit(w, digits, overline, index)
	case 2:
		outputDigit(w, digits, overline, index)
		outputDigit(w, digits, overline, index)
	case 1:
		outputDigit(w, digits, overline, index)
	}
}

func outputDigit(w io.Writer, digits string, overline string, digit int) {
	d := []byte{digits[digit]}
	w.Write(d)
	w.Write([]byte(overline))
}
