package intercal

import (
	"fmt"
)

var (
	Err000 = &Error{0, "An undecodable statement has been encountered in the course of execution.  Note that keypunching errors can be slightly disastrous, since if ‘FORGET’ were misspelled F-O-R-G-E-R, the results would probably not be those desired.  Extreme misspellings may have even more surprising consequences.  For example, misspelling ‘FORGET’ R-E-S-U-M-E could have drastic results.", "", nil, 0}
	Err017 = &Error{17, "An expression contains a syntax error.", "DO YOU EXPECT ME TO FIGURE THIS OUT?", nil, 0}
	Err079 = &Error{79, "Improper use has been made of statement identifiers.", "PROGRAMMER IS INSUFFICIENTLY POLITE", nil, 0}
	Err099 = &Error{99, "Improper use has been made of statement identifiers.", "PROGRAMMER IS OVERLY POLITE", nil, 0}
	Err123 = &Error{123, "Program has attempted 80 levels of NEXTing.", "PROGRAM HAS DISAPPEARED INTO THE BLACK LAGOON", nil, 0}
	Err129 = &Error{129, "Program has attempted to transfer to a non-existent line label.", "PROGRAM HAS GOTTEN LOST", nil, 0}
	Err139 = &Error{139, "An ABSTAIN or REINSTATE statement references a non-existent line label.", "I WASN'T PLANNING TO GO THERE ANYWAY", nil, 0}
	Err182 = &Error{182, "A line label has been multiply defined.", "YOU MUST LIKE THIS LABEL A LOT", nil, 0}
	Err197 = &Error{197, "An invalid line label has been encountered.", "SO!  65535 LABELS AREN'T ENOUGH FOR YOU?", nil, 0}
	Err200 = &Error{200, "An expression involves an unidentified variable.", "NOTHING VENTURED NOTHING GAINED", nil, 0}
	Err240 = &Error{240, "An attempt has been made to give an array a dimension of zero.", "ERROR HANDLER PRINTED SNIDE REMARK", nil, 0}
	Err241 = &Error{241, "Invalid dimensioning information was supplied in defining or using an array.", "VARIABLES MAY NOT BE STORED IN WEST HYPERSPACE", nil, 0}
	Err275 = &Error{275, "A 32-bit value has been assigned to a 16-bit variable.", "DON'T BYTE OFF MORE THAN YOU CAN CHEW", nil, 0}
	Err436 = &Error{436, "A retrieval has been attempted for an unSTASHed value.", "THROW STICK BEFORE RETRIEVING", nil, 0}
	Err533 = &Error{533, "A WRITE IN statement or interleave (¢) operation has produced a value requiring over 32 bits to represent.", "YOU WANT MAYBE WE SHOULD IMPLEMENT 64-BIT VARIABLES?", nil, 0}
	Err562 = &Error{562, "Insufficient data.", "I DO NOT COMPUTE", nil, 0}
	Err579 = &Error{579, "Input data is invalid.", "", nil, 0}
	Err621 = &Error{621, "The expression of a RESUME statement evaluated to #0.", "ERROR TYPE 621 ENCOUNTERED", nil, 0}
	Err632 = &Error{632, "Program execution was terminated via a RESUME statement instead of GIVE UP.", "PROGRAM ATTEMPTED TO EXIT WITHOUT ERROR MESSAGE", nil, 0}
	Err633 = &Error{633, "Execution has passed beyond the last statement of the program.", "PROGRAM FELL OFF THE EDGE", nil, 0}
	Err774 = &Error{774, "A compiler error has occurred (see section 8.1).", "RANDOM COMPILER BUG", nil, 0}
	Err778 = &Error{778, "An unexplainable compiler error has occurred (see J. Lyon or B. Woods).", "UNEXPLAINED COMPILER BUG", nil, 0}

	ErrGiveUp = &Error{-1, "", "", nil, 0}

	ErrorList = []*Error{
		Err000,
		Err017,
		Err079,
		Err099,
		Err123,
		Err129,
		Err139,
		Err182,
		Err197,
		Err200,
		Err240,
		Err241,
		Err275,
		Err436,
		Err533,
		Err562,
		Err579,
		Err621,
		Err632,
		Err633,
		Err774,
		Err778,
	}
)

type Error struct {
	code        int
	description string
	message     string
	statement   *Statement
	next        int
}

func (e *Error) Error() string {
	if e.next == 0 {
		if e.message != "" {
			return fmt.Sprintf("ICL%03dI %s\n\tON THE WAY TO STATEMENT nnnn\n\tCORRECT SOURCE AND RESUBMIT", e.code, e.message)
		} else if e.statement != nil {
			return fmt.Sprintf("ICL%03dI %s\n\tON THE WAY TO STATEMENT nnnn\n\tCORRECT SOURCE AND RESUBMIT", e.code, e.statement.String())
		} else {
			return fmt.Sprintf("ICL%03dI\n\tON THE WAY TO STATEMENT nnnn\n\tCORRECT SOURCE AND RESUBMIT", e.code)
		}
	} else {
		if e.message != "" {
			return fmt.Sprintf("ICL%03dI %s\n\tON THE WAY TO STATEMENT %04d\n\tCORRECT SOURCE AND RESUBMIT", e.code, e.message, e.next)
		} else if e.statement != nil {
			return fmt.Sprintf("ICL%03dI %s\n\tON THE WAY TO STATEMENT %04d\n\tCORRECT SOURCE AND RESUBMIT", e.code, e.statement.String(), e.next)
		} else {
			return fmt.Sprintf("ICL%03dI\n\tON THE WAY TO STATEMENT %04d\n\tCORRECT SOURCE AND RESUBMIT", e.code, e.next)
		}
	}
}

func (e *Error) At(state *State, statement *Statement) *Error {
	nextIndex := statement.Index + 1
	if statement.Type == StatementNext {
		if next, ok := statement.Operands.(int); ok {
			nextIndex = next
		}
	}
	for {
		if nextIndex >= len(state.Statements) {
			nextIndex = -1
			break
		}
		if !state.Statements[nextIndex].Not && state.Statements[nextIndex].Chance > 0 {
			break
		}
		nextIndex++
	}
	return &Error{
		code:        e.code,
		description: e.description,
		message:     e.message,
		statement:   statement,
		next:        nextIndex + 1,
	}
}

func (e *Error) WithMessage(message string) *Error {
	return &Error{
		code:        e.code,
		description: e.description,
		message:     message,
		statement:   e.statement,
		next:        e.next,
	}
}

func (e *Error) Code() int {
	return e.code
}

func (e *Error) Message() string {
	return e.message
}
