package intercal

import (
	"fmt"
	"io"
)

type LibraryFunction struct {
	Interp         func(state *State) *Error
	CollectVarInfo func(cgs *codeGenState)
	CodeGen        func(w io.Writer, cgs *codeGenState) error
}

var Library = map[uint16]LibraryFunction{
	1000: LibraryFunction{
		// .3 <- .1 plus .2, error exit on overflow
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1009: LibraryFunction{
		// .3 <- .1 plus .2
		// .4 <- #1 if no overflow, else .4 <- #2
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
			cgs.collectStmtVarInfo(Var16(4), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1010: LibraryFunction{
		// .3 <- .1 minus .2, no action on overflow
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1020: LibraryFunction{
		// .1 <- .1 plus #1, no action on overflow
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			varInfo := cgs.collectStmtVarInfo(Var16(1), StatementLibrary)
			varInfo.accessed = true
			varInfo.assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1030: LibraryFunction{
		// .3 <- .1 times .2, error exit on overflow
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1039: LibraryFunction{
		// .3 <- .1 times .2, error exit on overflow
		// .4 <- #1 if no overflow, else .4 <- #2
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
			cgs.collectStmtVarInfo(Var16(4), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1040: LibraryFunction{
		// .3 <- .1 divided by .2
		// .3 <- #0 if .2 is #0
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1050: LibraryFunction{
		// .2 <- :1 divided by .1, error exit on overflow
		// .2 <- #0 if .1 is #0
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).assigned = true
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1500: LibraryFunction{
		// :3 <- :1 plus :2, error exit on overflow
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1509: LibraryFunction{
		// :3 <- :1 plus :2
		// :4 <- #1 if no overflow, else :4 <- #2
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(3), StatementLibrary).assigned = true
			cgs.collectStmtVarInfo(Var32(4), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1510: LibraryFunction{
		// :3 <- :1 minus :2, no action on overflow
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1520: LibraryFunction{
		// :1 <- .1 concatenated with .2
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1530: LibraryFunction{
		// :1 <- .1 times .2
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1540: LibraryFunction{
		// :3 <- :1 times :2, error exit on overflow
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1549: LibraryFunction{
		// :3 <- :1 times :2
		// :4 <- #1 if no overflow, else :4 <- #2
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(3), StatementLibrary).assigned = true
			cgs.collectStmtVarInfo(Var32(4), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1550: LibraryFunction{
		// :3 <- :1 divided by :2
		// :3 <- #0 if :2 is #0
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1900: LibraryFunction{
		// .1 <- uniform random no. from #1 to #65535
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
	1910: LibraryFunction{
		// .2 <- normal random no. from #0 to .1, with
		// standard deviation .1 divided by #12
		Interp: func(state *State) *Error {
			//...
			return Err778
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			//...
			if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n", cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			return nil
		},
	},
}
