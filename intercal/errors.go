package main

import "errors"

var (
	ErrOverflow = errors.New("DON'T BYTE OFF MORE THAN YOU CAN CHEW.")

	Err000 = errors.New("000 An undecodable statement has been encountered in the course of execution.")
	Err017 = errors.New("017 An expression contains a syntax error.")
	Err079 = errors.New("079 Improper use has been made of statement identifiers.")
	Err099 = errors.New("099 Improper use has been made of statement identifiers.")
	Err123 = errors.New("123 Program has attempted 80 levels of NEXTing.")
	Err129 = errors.New("129 Program has attempted to transfer to a non-existent line label.")
	Err139 = errors.New("139 An ABSTAIN or REINSTATE statement references a non-existent line label.")
	Err182 = errors.New("182 A line label has been multiply defined.")
	Err197 = errors.New("197 An invalid line label has been encountered.")
	Err200 = errors.New("200 An expression involves an unidentified variable.")
	Err240 = errors.New("240 An attempt has been made to give an array a dimension of zero.")
	Err241 = errors.New("241 Invalid dimensioning information was supplied in defining or using an array.")
	Err275 = errors.New("275 A 32-bit value has been assigned to a 16-bit variable.")
	Err436 = errors.New("436 A retrieval has been attempted for an unSTASHed value.")
	Err533 = errors.New("533 A WRITE IN statement or interleave (¢) operation has produced a value requiring over 32 bits to represent.")
	Err562 = errors.New("562 Insufficient data.")
	Err579 = errors.New("579 Input data is invalid.")
	Err621 = errors.New("621 The expression of a RESUME statement evaluated to #0.")
	Err632 = errors.New("632 Program execution was terminated via a RESUME statement instead of GIVE UP.")
	Err633 = errors.New("633 Execution has passed beyond the last statement of the program.")
	Err774 = errors.New("774 A compiler error has occurred (see section 8.1).")
	Err778 = errors.New("778 An unexplainable compiler error has occurred (see J. Lyon or B. Woods).")
)
