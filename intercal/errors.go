package main

import (
	"fmt"
)

var (
	Err000 = &Error{"000", "An undecodable statement has been encountered in the course of execution.  Note that keypunching errors can be slightly disastrous, since if ‘FORGET’ were misspelled F-O-R-G-E-R, the results would probably not be those desired.  Extreme misspellings may have even more surprising consequences.  For example, misspelling ‘FORGET’ R-E-S-U-M-E could have drastic results.", "", 0}
	Err017 = &Error{"017", "An expression contains a syntax error.", "", 0}
	Err079 = &Error{"079", "Improper use has been made of statement identifiers.", "", 0}
	Err099 = &Error{"099", "Improper use has been made of statement identifiers.", "", 0}
	Err123 = &Error{"123", "Program has attempted 80 levels of NEXTing.", "PROGRAM HAS DISAPPEARED INTO THE BLACK LAGOON.", 0}
	Err129 = &Error{"129", "Program has attempted to transfer to a non-existent line label.", "", 0}
	Err139 = &Error{"139", "An ABSTAIN or REINSTATE statement references a non-existent line label.", "", 0}
	Err182 = &Error{"182", "A line label has been multiply defined.", "", 0}
	Err197 = &Error{"197", "An invalid line label has been encountered.", "", 0}
	Err200 = &Error{"200", "An expression involves an unidentified variable.", "", 0}
	Err240 = &Error{"240", "An attempt has been made to give an array a dimension of zero.", "", 0}
	Err241 = &Error{"241", "Invalid dimensioning information was supplied in defining or using an array.", "", 0}
	Err275 = &Error{"275", "A 32-bit value has been assigned to a 16-bit variable.", "DON'T BYTE OFF MORE THAN YOU CAN CHEW.", 0}
	Err436 = &Error{"436", "A retrieval has been attempted for an unSTASHed value.", "THROW STICK BEFORE RETRIEVING.", 0}
	Err533 = &Error{"533", "A WRITE IN statement or interleave (¢) operation has produced a value requiring over 32 bits to represent.", "", 0}
	Err562 = &Error{"562", "Insufficient data.", "", 0}
	Err579 = &Error{"579", "Input data is invalid.", "", 0}
	Err621 = &Error{"621", "The expression of a RESUME statement evaluated to #0.", "", 0}
	Err632 = &Error{"632", "Program execution was terminated via a RESUME statement instead of GIVE UP.", "", 0}
	Err633 = &Error{"633", "Execution has passed beyond the last statement of the program.", "", 0}
	Err774 = &Error{"774", "A compiler error has occurred (see section 8.1).", "", 0}
	Err778 = &Error{"778", "An unexplainable compiler error has occurred (see J. Lyon or B. Woods).", "", 0}
)

type Error struct {
	code      string
	message   string
	message2  string
	statement int
}

func (e *Error) Error() string {
	if e.statement == 0 {
		if e.message2 == "" {
			return fmt.Sprintf("ICL%sI (%s)", e.code, e.message)
		} else {
			return fmt.Sprintf("ICL%sI (%s)\n%s\n", e.code, e.message, e.message2)
		}
	} else {
		if e.message2 == "" {
			return fmt.Sprintf("ICL%sI (%s)\n        ON THE WAY TO STATEMENT %04d\n        CORRECT SOURCE AND RESUBMIT\n", e.code, e.message, e.statement)
		} else {
			return fmt.Sprintf("ICL%sI (%s)\n        ON THE WAY TO STATEMENT %04d\n        CORRECT SOURCE AND RESUBMIT\n%s\n", e.code, e.message, e.statement, e.message2)
		}
	}
}

func (e *Error) At(statementIndex int) *Error {
	return &Error{
		code:      e.code,
		message:   e.message,
		message2:  e.message2,
		statement: statementIndex + 1,
	}
}

func (e *Error) Code() string {
	return e.code
}
