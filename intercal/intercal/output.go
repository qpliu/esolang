package intercal

import (
	"io"
)

const (
	outputDigitsL = "ivxlcdm"
	outputDigitsU = "IVXLCDM"

	indexI = 0
	indexX = 2
	indexL = 3
	indexC = 4
	indexD = 5
	indexM = 6

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
	outputTens(w, digits, overline, indexM, value/1000)
	if value%1000 == 999 {
		outputDigit(w, digits, overline, indexI)
		outputDigit(w, digits, overline, indexM)
		return
	}
	if value%1000 >= 990 {
		outputDigit(w, digits, overline, indexX)
		outputDigit(w, digits, overline, indexM)
		outputTens(w, digits, overline, indexI, value%10)
		return
	}
	if value%1000 == 499 {
		outputDigit(w, digits, overline, indexI)
		outputDigit(w, digits, overline, indexD)
		return
	}
	if value%1000 >= 490 && value%1000 < 500 {
		outputDigit(w, digits, overline, indexX)
		outputDigit(w, digits, overline, indexD)
		outputTens(w, digits, overline, indexI, value%10)
		return
	}
	outputTens(w, digits, overline, indexC, (value/100)%10)
	if value%100 == 99 {
		outputDigit(w, digits, overline, indexI)
		outputDigit(w, digits, overline, indexC)
		return
	}
	if value%100 == 49 {
		outputDigit(w, digits, overline, indexI)
		outputDigit(w, digits, overline, indexL)
		return
	}
	outputTens(w, digits, overline, indexX, (value/10)%10)
	outputTens(w, digits, overline, indexI, value%10)
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
