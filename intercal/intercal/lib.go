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
			v3 := state.Var16(Var16(3))
			if v3.Ignored {
				return nil
			}
			v1 := state.Var16(Var16(1))
			v2 := state.Var16(Var16(2))
			v3.Value = v1.Value + v2.Value
			if v3.Value >= 65536 {
				return Err1999
			}
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			doneLabel := cgs.label("lib1000_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(3)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1000_spot3_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v1ValIdent := cgs.ident("spot1_val")
			if err := cgs.genAccessVar(w, v1ValIdent, cgs.varInfo(Var16(1))); err != nil {
				return err
			}
			v1Ident := cgs.ident("spot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v1Ident, v1ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v2ValIdent := cgs.ident("spot2_val")
			if err := cgs.genAccessVar(w, v2ValIdent, cgs.varInfo(Var16(2))); err != nil {
				return err
			}
			v2Ident := cgs.ident("spot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v2Ident, v2ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			sumIdent := cgs.ident("sum")
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,%s%s\n", sumIdent, v1Ident, v2Ident, cgs.debugLocation); err != nil {
				return err
			}
			overflowCheckIdent := cgs.ident("overflow_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,65536%s\n", overflowCheckIdent, sumIdent, cgs.debugLocation); err != nil {
				return err
			}
			overflowLabel := cgs.label("lib1000_overflow")
			storev3Label := cgs.label("lib1000_spot3_gets")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", overflowCheckIdent, overflowLabel, storev3Label, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 1999,1),i32 %d)%s\n    br label %%%s%s\n", overflowLabel, cgs.nextIndex(), cgs.debugLocation, doneLabel, cgs.debugLocation); err != nil {
				return err
			}
			if !cgs.varInfo(Var16(3)).accessed {
				if _, err := fmt.Fprintf(w, "  %s:\n    br label %%%s%s\n", storev3Label, doneLabel, cgs.debugLocation); err != nil {
					return err
				}
			} else {
				sumValIdent := cgs.ident("sum_val")
				if _, err := fmt.Fprintf(w, "  %s:\n    %s = insertvalue %%val zeroinitializer,i32 %s,1%s\n", storev3Label, sumValIdent, sumIdent, cgs.debugLocation); err != nil {
					return err
				}
				if err := cgs.genGets(w, Var16(3), sumValIdent, doneLabel); err != nil {
					return err
				}
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
	1009: LibraryFunction{
		// .3 <- .1 plus .2
		// .4 <- #1 if no overflow, else .4 <- #2
		Interp: func(state *State) *Error {
			v1 := state.Var16(Var16(1))
			v2 := state.Var16(Var16(2))
			v3 := state.Var16(Var16(3))
			if !v3.Ignored {
				v3.Value = (v1.Value + v2.Value) & 65535
			}
			v4 := state.Var16(Var16(4))
			if !v4.Ignored {
				if v1.Value+v2.Value >= 65536 {
					v4.Value = 2
				} else {
					v4.Value = 1
				}
			}
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
			cgs.collectStmtVarInfo(Var16(4), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			v3Info := cgs.varInfo(Var16(3))
			v4Info := cgs.varInfo(Var16(4))
			if !v3Info.accessed && !v4Info.accessed {
				return nil
			}
			v1ValIdent := cgs.ident("spot1_val")
			if err := cgs.genAccessVar(w, v1ValIdent, cgs.varInfo(Var16(1))); err != nil {
				return err
			}
			v1Ident := cgs.ident("spot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v1Ident, v1ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v2ValIdent := cgs.ident("spot2_val")
			if err := cgs.genAccessVar(w, v2ValIdent, cgs.varInfo(Var16(2))); err != nil {
				return err
			}
			v2Ident := cgs.ident("spot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v2Ident, v2ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			sumIdent := cgs.ident("sum")
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,%s%s\n", sumIdent, v1Ident, v2Ident, cgs.debugLocation); err != nil {
				return err
			}
			if v3Info.accessed {
				doneLabel := cgs.label("lib1009_spot3_gets_done")
				if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(3)); err != nil {
					return err
				} else {
					notIgnoredLabel := cgs.label("lib1009_spot3_not_ignored")
					if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
						return err
					}
				}
				sum16Ident := cgs.ident("sum_16bit")
				if _, err := fmt.Fprintf(w, "    %s = and i32 %s,65535%s\n", sum16Ident, sumIdent, cgs.debugLocation); err != nil {
					return err
				}
				sumValIdent := cgs.ident("sum_val")
				if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val zeroinitializer,i32 %s,1%s\n", sumValIdent, sum16Ident, cgs.debugLocation); err != nil {
					return err
				}
				if err := cgs.genGets(w, Var16(3), sumValIdent, doneLabel); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
					return err
				}
			}
			if v4Info.accessed {
				doneLabel := cgs.label("lib1009_spot4_gets_done")
				if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(4)); err != nil {
					return err
				} else {
					notIgnoredLabel := cgs.label("lib1009_spot4_not_ignored")
					if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
						return err
					}
				}
				overflowCheckIdent := cgs.ident("overflow_check")
				if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,65536%s\n", overflowCheckIdent, sumIdent, cgs.debugLocation); err != nil {
					return err
				}
				overflowValIdent := cgs.ident("overflow_val")
				if _, err := fmt.Fprintf(w, "    %s = select i1 %s,%%val insertvalue(%%val zeroinitializer,i32 2,1),%%val insertvalue(%%val zeroinitializer,i32 1,1)%s\n", overflowValIdent, overflowCheckIdent, cgs.debugLocation); err != nil {
					return err
				}
				if err := cgs.genGets(w, Var16(4), overflowValIdent, doneLabel); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
					return err
				}
			}
			return nil
		},
	},
	1010: LibraryFunction{
		// .3 <- .1 minus .2, no action on overflow
		Interp: func(state *State) *Error {
			v3 := state.Var16(Var16(3))
			if v3.Ignored {
				return nil
			}
			v1 := state.Var16(Var16(1))
			v2 := state.Var16(Var16(2))
			v3.Value = (v1.Value - v2.Value) & 65535
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			if !cgs.varInfo(Var16(3)).accessed {
				return nil
			}
			doneLabel := cgs.label("lib1010_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(3)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1010_spot3_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v1ValIdent := cgs.ident("spot1_val")
			if err := cgs.genAccessVar(w, v1ValIdent, cgs.varInfo(Var16(1))); err != nil {
				return err
			}
			v1Ident := cgs.ident("spot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v1Ident, v1ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v2ValIdent := cgs.ident("spot2_val")
			if err := cgs.genAccessVar(w, v2ValIdent, cgs.varInfo(Var16(2))); err != nil {
				return err
			}
			v2Ident := cgs.ident("spot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v2Ident, v2ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			diffIdent := cgs.ident("diff")
			if _, err := fmt.Fprintf(w, "    %s = sub i32 %s,%s%s\n", diffIdent, v1Ident, v2Ident, cgs.debugLocation); err != nil {
				return err
			}
			diff16Ident := cgs.ident("diff_16bit")
			if _, err := fmt.Fprintf(w, "    %s = and i32 %s,65535%s\n", diff16Ident, diffIdent, cgs.debugLocation); err != nil {
				return err
			}
			diffValIdent := cgs.ident("diff_val")
			if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val zeroinitializer,i32 %s,1%s\n", diffValIdent, diff16Ident, cgs.debugLocation); err != nil {
				return err
			}
			if err := cgs.genGets(w, Var16(3), diffValIdent, doneLabel); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
	1020: LibraryFunction{
		// .1 <- .1 plus #1, no action on overflow
		Interp: func(state *State) *Error {
			v1 := state.Var16(Var16(1))
			if !v1.Ignored {
				v1.Value++
			}
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			varInfo := cgs.collectStmtVarInfo(Var16(1), StatementLibrary)
			varInfo.accessed = true
			varInfo.assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			doneLabel := cgs.label("lib1020_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(1)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1020_spot1_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v1ValIdent := cgs.ident("spot1_val")
			if err := cgs.genAccessVar(w, v1ValIdent, cgs.varInfo(Var16(1))); err != nil {
				return err
			}
			v1Ident := cgs.ident("spot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v1Ident, v1ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			incIdent := cgs.ident("inc")
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", incIdent, v1Ident, cgs.debugLocation); err != nil {
				return err
			}
			incValIdent := cgs.ident("inc_val")
			if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val zeroinitializer,i32 %s,1%s\n", incValIdent, incIdent, cgs.debugLocation); err != nil {
				return err
			}
			if err := cgs.genGets(w, Var16(1), incValIdent, doneLabel); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
	1030: LibraryFunction{
		// .3 <- .1 times .2, error exit on overflow
		Interp: func(state *State) *Error {
			v3 := state.Var16(Var16(3))
			if v3.Ignored {
				return nil
			}
			v1 := state.Var16(Var16(1))
			v2 := state.Var16(Var16(2))
			v3.Value = v1.Value * v2.Value
			if v3.Value >= 65536 {
				return Err1999
			}
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			doneLabel := cgs.label("lib1030_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(3)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1030_spot3_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v1ValIdent := cgs.ident("spot1_val")
			if err := cgs.genAccessVar(w, v1ValIdent, cgs.varInfo(Var16(1))); err != nil {
				return err
			}
			v1Ident := cgs.ident("spot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v1Ident, v1ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v2ValIdent := cgs.ident("spot2_val")
			if err := cgs.genAccessVar(w, v2ValIdent, cgs.varInfo(Var16(2))); err != nil {
				return err
			}
			v2Ident := cgs.ident("spot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v2Ident, v2ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			prodIdent := cgs.ident("prod")
			if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s%s\n", prodIdent, v1Ident, v2Ident, cgs.debugLocation); err != nil {
				return err
			}
			overflowCheckIdent := cgs.ident("overflow_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,65536%s\n", overflowCheckIdent, prodIdent, cgs.debugLocation); err != nil {
				return err
			}
			overflowLabel := cgs.label("lib1030_overflow")
			storev3Label := cgs.label("lib1030_spot3_gets")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", overflowCheckIdent, overflowLabel, storev3Label, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 1999,1),i32 %d)%s\n    br label %%%s%s\n", overflowLabel, cgs.nextIndex(), cgs.debugLocation, doneLabel, cgs.debugLocation); err != nil {
				return err
			}
			if !cgs.varInfo(Var16(3)).accessed {
				if _, err := fmt.Fprintf(w, "  %s:\n    br label %%%s%s\n", storev3Label, doneLabel, cgs.debugLocation); err != nil {
					return err
				}
			} else {
				prodValIdent := cgs.ident("prod_val")
				if _, err := fmt.Fprintf(w, "  %s:\n    %s = insertvalue %%val zeroinitializer,i32 %s,1%s\n", storev3Label, prodValIdent, prodIdent, cgs.debugLocation); err != nil {
					return err
				}
				if err := cgs.genGets(w, Var16(3), prodValIdent, doneLabel); err != nil {
					return err
				}
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
	1039: LibraryFunction{
		// .3 <- .1 times .2, error exit on overflow
		// .4 <- #1 if no overflow, else .4 <- #2
		Interp: func(state *State) *Error {
			v1 := state.Var16(Var16(1))
			v2 := state.Var16(Var16(2))
			v3 := state.Var16(Var16(3))
			if !v3.Ignored {
				v3.Value = (v1.Value * v2.Value) & 65535
			}
			v4 := state.Var16(Var16(4))
			if !v4.Ignored {
				if v1.Value*v2.Value >= 65536 {
					v4.Value = 2
				} else {
					v4.Value = 1
				}
			}
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
			cgs.collectStmtVarInfo(Var16(4), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			v3Info := cgs.varInfo(Var16(3))
			v4Info := cgs.varInfo(Var16(4))
			if !v3Info.accessed && !v4Info.accessed {
				return nil
			}
			v1ValIdent := cgs.ident("spot1_val")
			if err := cgs.genAccessVar(w, v1ValIdent, cgs.varInfo(Var16(1))); err != nil {
				return err
			}
			v1Ident := cgs.ident("spot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v1Ident, v1ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v2ValIdent := cgs.ident("spot2_val")
			if err := cgs.genAccessVar(w, v2ValIdent, cgs.varInfo(Var16(2))); err != nil {
				return err
			}
			v2Ident := cgs.ident("spot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v2Ident, v2ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			prodIdent := cgs.ident("prod")
			if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s%s\n", prodIdent, v1Ident, v2Ident, cgs.debugLocation); err != nil {
				return err
			}
			if v3Info.accessed {
				doneLabel := cgs.label("lib1039_spot3_gets_done")
				if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(3)); err != nil {
					return err
				} else {
					notIgnoredLabel := cgs.label("lib1039_spot3_not_ignored")
					if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
						return err
					}
				}
				prod16Ident := cgs.ident("prod_16bit")
				if _, err := fmt.Fprintf(w, "    %s = and i32 %s,65535%s\n", prod16Ident, prodIdent, cgs.debugLocation); err != nil {
					return err
				}
				prodValIdent := cgs.ident("prod_val")
				if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val zeroinitializer,i32 %s,1%s\n", prodValIdent, prod16Ident, cgs.debugLocation); err != nil {
					return err
				}
				if err := cgs.genGets(w, Var16(3), prodValIdent, doneLabel); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
					return err
				}
			}
			if v4Info.accessed {
				doneLabel := cgs.label("lib1039_spot4_gets_done")
				if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(4)); err != nil {
					return err
				} else {
					notIgnoredLabel := cgs.label("lib1039_spot4_not_ignored")
					if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
						return err
					}
				}
				overflowCheckIdent := cgs.ident("overflow_check")
				if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,65536%s\n", overflowCheckIdent, prodIdent, cgs.debugLocation); err != nil {
					return err
				}
				overflowValIdent := cgs.ident("overflow_val")
				if _, err := fmt.Fprintf(w, "    %s = select i1 %s,%%val insertvalue(%%val zeroinitializer,i32 2,1),%%val insertvalue(%%val zeroinitializer,i32 1,1)%s\n", overflowValIdent, overflowCheckIdent, cgs.debugLocation); err != nil {
					return err
				}
				if err := cgs.genGets(w, Var16(4), overflowValIdent, doneLabel); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
					return err
				}
			}
			return nil
		},
	},
	1040: LibraryFunction{
		// .3 <- .1 divided by .2
		// .3 <- #0 if .2 is #0
		Interp: func(state *State) *Error {
			v3 := state.Var16(Var16(3))
			if v3.Ignored {
				return nil
			}
			v1 := state.Var16(Var16(1))
			v2 := state.Var16(Var16(2))
			if v2.Value == 0 {
				v3.Value = 0
			} else {
				v3.Value = v1.Value / v2.Value
			}
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			if !cgs.varInfo(Var16(3)).accessed {
				return nil
			}
			doneLabel := cgs.label("lib1040_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(3)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1040_spot3_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v1ValIdent := cgs.ident("spot1_val")
			if err := cgs.genAccessVar(w, v1ValIdent, cgs.varInfo(Var16(1))); err != nil {
				return err
			}
			v1Ident := cgs.ident("spot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v1Ident, v1ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v2ValIdent := cgs.ident("spot2_val")
			if err := cgs.genAccessVar(w, v2ValIdent, cgs.varInfo(Var16(2))); err != nil {
				return err
			}
			v2Ident := cgs.ident("spot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v2Ident, v2ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v2zeroIdent := cgs.ident("spot2_is_zero")
			if _, err := fmt.Fprintf(w, "    %s = icmp eq i32 %s,0%s\n", v2zeroIdent, v2Ident, cgs.debugLocation); err != nil {
				return err
			}
			checkv2Label := cgs.label("lib1040_check_spot2_zero")
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", checkv2Label, cgs.debugLocation, checkv2Label); err != nil {
				return err
			}
			v3getsLabel := cgs.label("lib1040_spot3_gets")
			divLabel := cgs.label("lib1040_div")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", v2zeroIdent, v3getsLabel, divLabel, cgs.debugLocation, divLabel); err != nil {
				return err
			}
			quotIdent := cgs.ident("quot")
			if _, err := fmt.Fprintf(w, "    %s = udiv i32 %s,%s%s\n", quotIdent, v1Ident, v2Ident, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", v3getsLabel, cgs.debugLocation, v3getsLabel); err != nil {
				return err
			}
			quotPhiIdent := cgs.ident("quot_phi")
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s],[%s,%%%s]%s\n", quotPhiIdent, checkv2Label, quotIdent, divLabel, cgs.debugLocation); err != nil {
				return err
			}
			quotValIdent := cgs.ident("quot_val")
			if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val zeroinitializer,i32 %s,1%s\n", quotValIdent, quotPhiIdent, cgs.debugLocation); err != nil {
				return err
			}
			if err := cgs.genGets(w, Var16(3), quotValIdent, doneLabel); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
	1050: LibraryFunction{
		// .2 <- :1 divided by .1, error exit on overflow
		// .2 <- #0 if .1 is #0
		Interp: func(state *State) *Error {
			v2 := state.Var16(Var16(2))
			if v2.Ignored {
				return nil
			}
			v01 := state.Var32(Var32(1))
			v1 := state.Var16(Var16(1))
			if v1.Value == 0 {
				v2.Value = 0
			} else {
				v2.Value = v01.Value / v1.Value
				if v2.Value >= 65536 {
					return Err1999
				}
			}
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).assigned = true
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			doneLabel := cgs.label("lib1050_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(2)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1050_spot2_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v1ValIdent := cgs.ident("spot1_val")
			if err := cgs.genAccessVar(w, v1ValIdent, cgs.varInfo(Var16(1))); err != nil {
				return err
			}
			v1Ident := cgs.ident("spot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v1Ident, v1ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			checkv1ZeroLabel := cgs.label("lib1050_check_spot1_zero")
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", checkv1ZeroLabel, cgs.debugLocation, checkv1ZeroLabel); err != nil {
				return err
			}
			v1zeroIdent := cgs.ident("spot1_is_zero")
			if _, err := fmt.Fprintf(w, "    %s = icmp eq i32 %s,0%s\n", v1zeroIdent, v1Ident, cgs.debugLocation); err != nil {
				return err
			}
			v2getsLabel := cgs.label("lib1050_spot2_gets")
			accessv01Label := cgs.label("lib1050_twospot1")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", v1zeroIdent, v2getsLabel, accessv01Label, cgs.debugLocation, accessv01Label); err != nil {
				return err
			}
			v01ValIdent := cgs.ident("twospot1_val")
			if err := cgs.genAccessVar(w, v01ValIdent, cgs.varInfo(Var32(1))); err != nil {
				return err
			}
			v01Ident := cgs.ident("twospot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v01Ident, v01ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			divLabel := cgs.label("lib1050_div")
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", divLabel, cgs.debugLocation, divLabel); err != nil {
				return err
			}
			quotIdent := cgs.ident("quot")
			if _, err := fmt.Fprintf(w, "    %s = udiv i32 %s,%s%s\n", quotIdent, v01Ident, v1Ident, cgs.debugLocation); err != nil {
				return err
			}
			overflowIdent := cgs.ident("overflow")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,65536%s\n", overflowIdent, quotIdent, cgs.debugLocation); err != nil {
				return err
			}
			overflowLabel := cgs.label("lib1050_overflow")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", overflowIdent, overflowLabel, v2getsLabel, cgs.debugLocation, v2getsLabel); err != nil {
				return err
			}
			if !cgs.varInfo(Var16(2)).accessed {
				if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", doneLabel, cgs.debugLocation); err != nil {
					return err
				}
			} else {
				quotPhiIdent := cgs.ident("quot_phi")
				if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s],[%s,%%%s]%s\n", quotPhiIdent, checkv1ZeroLabel, quotIdent, divLabel, cgs.debugLocation); err != nil {
					return err
				}
				quotValIdent := cgs.ident("quot_val")
				if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val zeroinitializer,i32 %s,1%s\n", quotValIdent, quotPhiIdent, cgs.debugLocation); err != nil {
					return err
				}
				if err := cgs.genGets(w, Var16(2), quotValIdent, doneLabel); err != nil {
					return err
				}
			}
			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 1999,1),i32 %d)%s\n    br label %%%s%s\n", overflowLabel, cgs.nextIndex(), cgs.debugLocation, doneLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
	1500: LibraryFunction{
		// :3 <- :1 plus :2, error exit on overflow
		Interp: func(state *State) *Error {
			v03 := state.Var32(Var32(3))
			if v03.Ignored {
				return nil
			}
			v01 := state.Var32(Var32(1))
			v02 := state.Var32(Var32(2))
			v03.Value = v01.Value + v02.Value
			if v03.Value < v01.Value {
				return Err1999
			}
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			doneLabel := cgs.label("lib1500_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var32(3)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1500_twospot3_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v01ValIdent := cgs.ident("twospot1_val")
			if err := cgs.genAccessVar(w, v01ValIdent, cgs.varInfo(Var32(1))); err != nil {
				return err
			}
			v01Ident := cgs.ident("twospot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v01Ident, v01ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v02ValIdent := cgs.ident("twospot2_val")
			if err := cgs.genAccessVar(w, v02ValIdent, cgs.varInfo(Var32(2))); err != nil {
				return err
			}
			v02Ident := cgs.ident("twospot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v02Ident, v02ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			sumIdent := cgs.ident("sum")
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,%s%s\n", sumIdent, v01Ident, v02Ident, cgs.debugLocation); err != nil {
				return err
			}
			overflowCheckIdent := cgs.ident("overflow_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp ult i32 %s,%s%s\n", overflowCheckIdent, sumIdent, v01Ident, cgs.debugLocation); err != nil {
				return err
			}
			overflowLabel := cgs.label("lib1500_overflow")
			storev03Label := cgs.label("lib1500_twospot3_gets")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", overflowCheckIdent, overflowLabel, storev03Label, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 1999,1),i32 %d)%s\n    br label %%%s%s\n", overflowLabel, cgs.nextIndex(), cgs.debugLocation, doneLabel, cgs.debugLocation); err != nil {
				return err
			}
			if !cgs.varInfo(Var32(3)).accessed {
				if _, err := fmt.Fprintf(w, "  %s:\n    br label %%%s%s\n", storev03Label, doneLabel, cgs.debugLocation); err != nil {
					return err
				}
			} else {
				sumValIdent := cgs.ident("sum_val")
				if _, err := fmt.Fprintf(w, "  %s:\n    %s = insertvalue %%val insertvalue(%%val zeroinitializer,i2 1,0),i32 %s,1%s\n", storev03Label, sumValIdent, sumIdent, cgs.debugLocation); err != nil {
					return err
				}
				if err := cgs.genGets(w, Var32(3), sumValIdent, doneLabel); err != nil {
					return err
				}
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
	1509: LibraryFunction{
		// :3 <- :1 plus :2
		// :4 <- #1 if no overflow, else :4 <- #2
		Interp: func(state *State) *Error {
			v01 := state.Var32(Var32(1))
			v02 := state.Var32(Var32(2))
			v03 := state.Var32(Var32(3))
			if !v03.Ignored {
				v03.Value = v01.Value + v02.Value
			}
			v04 := state.Var32(Var32(4))
			if !v04.Ignored {
				if v03.Value < v01.Value {
					v04.Value = 2
				} else {
					v04.Value = 1
				}
			}
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(3), StatementLibrary).assigned = true
			cgs.collectStmtVarInfo(Var32(4), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			v03Info := cgs.varInfo(Var32(3))
			v04Info := cgs.varInfo(Var32(4))
			if !v03Info.accessed && !v04Info.accessed {
				return nil
			}
			v01ValIdent := cgs.ident("twospot1_val")
			if err := cgs.genAccessVar(w, v01ValIdent, cgs.varInfo(Var32(1))); err != nil {
				return err
			}
			v01Ident := cgs.ident("twospot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v01Ident, v01ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v02ValIdent := cgs.ident("twospot2_val")
			if err := cgs.genAccessVar(w, v02ValIdent, cgs.varInfo(Var32(2))); err != nil {
				return err
			}
			v02Ident := cgs.ident("twospot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v02Ident, v02ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			sumIdent := cgs.ident("sum")
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,%s%s\n", sumIdent, v01Ident, v02Ident, cgs.debugLocation); err != nil {
				return err
			}
			if v03Info.accessed {
				doneLabel := cgs.label("lib1509_twospot3_gets_done")
				if ignoredIdent, err := cgs.genCheckIgnored(w, Var32(3)); err != nil {
					return err
				} else {
					notIgnoredLabel := cgs.label("lib1509_twospot3_not_ignored")
					if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
						return err
					}
				}
				sumValIdent := cgs.ident("sum_val")
				if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val insertvalue(%%val zeroinitializer,i2 1,0),i32 %s,1%s\n", sumValIdent, sumIdent, cgs.debugLocation); err != nil {
					return err
				}
				if err := cgs.genGets(w, Var32(3), sumValIdent, doneLabel); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
					return err
				}
			}
			if v04Info.accessed {
				doneLabel := cgs.label("lib1509_twospot4_gets_done")
				if ignoredIdent, err := cgs.genCheckIgnored(w, Var32(4)); err != nil {
					return err
				} else {
					notIgnoredLabel := cgs.label("lib1509_twospot4_not_ignored")
					if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
						return err
					}
				}
				overflowCheckIdent := cgs.ident("overflow_check")
				if _, err := fmt.Fprintf(w, "    %s = icmp ult i32 %s,%s%s\n", overflowCheckIdent, sumIdent, v01Ident, cgs.debugLocation); err != nil {
					return err
				}
				overflowValIdent := cgs.ident("overflow_val")
				if _, err := fmt.Fprintf(w, "    %s = select i1 %s,%%val insertvalue(%%val zeroinitializer,i32 2,1),%%val insertvalue(%%val zeroinitializer,i32 1,1)%s\n", overflowValIdent, overflowCheckIdent, cgs.debugLocation); err != nil {
					return err
				}
				if err := cgs.genGets(w, Var32(4), overflowValIdent, doneLabel); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
					return err
				}
			}
			return nil
		},
	},
	1510: LibraryFunction{
		// :3 <- :1 minus :2, no action on overflow
		Interp: func(state *State) *Error {
			v03 := state.Var32(Var32(3))
			if v03.Ignored {
				return nil
			}
			v01 := state.Var32(Var32(1))
			v02 := state.Var32(Var32(2))
			v03.Value = v01.Value - v02.Value
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			if !cgs.varInfo(Var32(3)).accessed {
				return nil
			}
			doneLabel := cgs.label("lib1510_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var32(3)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1510_twospot3_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v01ValIdent := cgs.ident("twospot1_val")
			if err := cgs.genAccessVar(w, v01ValIdent, cgs.varInfo(Var32(1))); err != nil {
				return err
			}
			v01Ident := cgs.ident("twospot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v01Ident, v01ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v02ValIdent := cgs.ident("twospot2_val")
			if err := cgs.genAccessVar(w, v02ValIdent, cgs.varInfo(Var32(2))); err != nil {
				return err
			}
			v02Ident := cgs.ident("twospot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v02Ident, v02ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			diffIdent := cgs.ident("diff")
			if _, err := fmt.Fprintf(w, "    %s = sub i32 %s,%s%s\n", diffIdent, v01Ident, v02Ident, cgs.debugLocation); err != nil {
				return err
			}
			diffValIdent := cgs.ident("diff_val")
			if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val insertvalue(%%val zeroinitializer,i2 1,0),i32 %s,1%s\n", diffValIdent, diffIdent, cgs.debugLocation); err != nil {
				return err
			}
			if err := cgs.genGets(w, Var32(3), diffValIdent, doneLabel); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
	1520: LibraryFunction{
		// :1 <- .1 concatenated with .2
		Interp: func(state *State) *Error {
			v01 := state.Var32(Var32(1))
			if v01.Ignored {
				return nil
			}
			v1 := state.Var16(Var16(1))
			v2 := state.Var16(Var16(2))
			v01.Value = v1.Value<<16 | v2.Value
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			if !cgs.varInfo(Var32(1)).accessed {
				return nil
			}
			doneLabel := cgs.label("lib1520_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var32(1)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1520_twospot1_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v1ValIdent := cgs.ident("spot1_val")
			if err := cgs.genAccessVar(w, v1ValIdent, cgs.varInfo(Var16(1))); err != nil {
				return err
			}
			v1Ident := cgs.ident("spot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v1Ident, v1ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v1shiftIdent := cgs.ident("spot1_shift")
			if _, err := fmt.Fprintf(w, "    %s = shl i32 %s,16%s\n", v1shiftIdent, v1Ident, cgs.debugLocation); err != nil {
				return err
			}
			v2ValIdent := cgs.ident("spot2_val")
			if err := cgs.genAccessVar(w, v2ValIdent, cgs.varInfo(Var16(2))); err != nil {
				return err
			}
			v2Ident := cgs.ident("spot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v2Ident, v2ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			concatIdent := cgs.ident("concat")
			if _, err := fmt.Fprintf(w, "    %s = or i32 %s,%s%s\n", concatIdent, v1shiftIdent, v2Ident, cgs.debugLocation); err != nil {
				return err
			}
			concatValIdent := cgs.ident("concat_val")
			if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val insertvalue(%%val zeroinitializer,i2 1,0),i32 %s,1%s\n", concatValIdent, concatIdent, cgs.debugLocation); err != nil {
				return err
			}
			if err := cgs.genGets(w, Var32(1), concatValIdent, doneLabel); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
	1530: LibraryFunction{
		// :1 <- .1 times .2
		Interp: func(state *State) *Error {
			v01 := state.Var32(Var32(1))
			if v01.Ignored {
				return nil
			}
			v1 := state.Var16(Var16(1))
			v2 := state.Var16(Var16(2))
			v01.Value = v1.Value * v2.Value
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			if !cgs.varInfo(Var32(1)).accessed {
				return nil
			}
			doneLabel := cgs.label("lib1530_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var32(1)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1530_twospot1_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v1ValIdent := cgs.ident("spot1_val")
			if err := cgs.genAccessVar(w, v1ValIdent, cgs.varInfo(Var16(1))); err != nil {
				return err
			}
			v1Ident := cgs.ident("spot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v1Ident, v1ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v2ValIdent := cgs.ident("spot2_val")
			if err := cgs.genAccessVar(w, v2ValIdent, cgs.varInfo(Var16(2))); err != nil {
				return err
			}
			v2Ident := cgs.ident("spot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v2Ident, v2ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			prodIdent := cgs.ident("prod")
			if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s%s\n", prodIdent, v1Ident, v2Ident, cgs.debugLocation); err != nil {
				return err
			}
			prodValIdent := cgs.ident("prod_val")
			if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val insertvalue(%%val zeroinitializer,i2 1,0),i32 %s,1%s\n", prodValIdent, prodIdent, cgs.debugLocation); err != nil {
				return err
			}
			if err := cgs.genGets(w, Var32(1), prodValIdent, doneLabel); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
	1540: LibraryFunction{
		// :3 <- :1 times :2, error exit on overflow
		Interp: func(state *State) *Error {
			v03 := state.Var32(Var32(3))
			if v03.Ignored {
				return nil
			}
			v01 := state.Var32(Var32(1))
			v02 := state.Var32(Var32(2))
			l := v01.Value
			r := v02.Value
			if l > r {
				l, r = r, l
			}
			if l >= 65536 {
				return Err1999
			}
			hi := (r >> 16) * l
			if hi >= 65536 {
				return Err1999
			}
			hi <<= 16
			lo := (r & 65535) * l
			v03.Value = hi + lo
			if v03.Value < hi {
				return Err1999
			}
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var32(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(2), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var32(3), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			doneLabel := cgs.label("lib1540_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var32(3)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1540_twospot3_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v01ValIdent := cgs.ident("twospot1_val")
			if err := cgs.genAccessVar(w, v01ValIdent, cgs.varInfo(Var32(1))); err != nil {
				return err
			}
			v01Ident := cgs.ident("twospot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v01Ident, v01ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			v02ValIdent := cgs.ident("twospot2_val")
			if err := cgs.genAccessVar(w, v02ValIdent, cgs.varInfo(Var32(2))); err != nil {
				return err
			}
			v02Ident := cgs.ident("twospot2_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v02Ident, v02ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			overflowLabel := cgs.label("overflow")
			v01bigIdent := cgs.ident("twospot1_big")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,65536%s\n", v01bigIdent, v01Ident, cgs.debugLocation); err != nil {
				return err
			}
			v02bigIdent := cgs.ident("twospot2_big")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,65536%s\n", v02bigIdent, v02Ident, cgs.debugLocation); err != nil {
				return err
			}
			bothBigIdent := cgs.ident("both_big")
			if _, err := fmt.Fprintf(w, "    %s = and i1 %s,%s%s\n", bothBigIdent, v01bigIdent, v02bigIdent, cgs.debugLocation); err != nil {
				return err
			}
			check01bigLabel := cgs.label("check_twospot1_big")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", bothBigIdent, overflowLabel, check01bigLabel, cgs.debugLocation); err != nil {
				return err
			}
			check02bigLabel := cgs.label("check_twospot2_big")
			mulBigLabel := cgs.label("mul_big")
			if _, err := fmt.Fprintf(w, "  %s:\n    br i1 %s,label %%%s,label %%%s%s\n", check01bigLabel, v01bigIdent, mulBigLabel, check02bigLabel, cgs.debugLocation); err != nil {
				return err
			}
			mulSmallLabel := cgs.label("mul_small")
			if _, err := fmt.Fprintf(w, "  %s:\n    br i1 %s,label %%%s,label %%%s%s\n", check02bigLabel, v02bigIdent, mulBigLabel, mulSmallLabel, cgs.debugLocation); err != nil {
				return err
			}

			v03getsLabel := cgs.label("twospot3_gets")
			prodSmallIdent := cgs.ident("prod_small")
			if _, err := fmt.Fprintf(w, "  %s:\n    %s = mul i32 %s,%s%s\n    br label %%%s%s\n", mulSmallLabel, prodSmallIdent, v01Ident, v02Ident, cgs.debugLocation, v03getsLabel, cgs.debugLocation); err != nil {
				return err
			}

			if _, err := fmt.Fprintf(w, "  %s:\n", mulBigLabel); err != nil {
				return err
			}
			factorBigIdent := cgs.ident("factor_big")
			factorSmallIdent := cgs.ident("factor_small")
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [%s,%%%s],[%s,%%%s]%s\n", factorBigIdent, v01Ident, check01bigLabel, v02Ident, check02bigLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [%s,%%%s],[%s,%%%s]%s\n", factorSmallIdent, v02Ident, check01bigLabel, v01Ident, check02bigLabel, cgs.debugLocation); err != nil {
				return err
			}
			factorBigHiIdent := cgs.ident("factor_big_hi")
			if _, err := fmt.Fprintf(w, "    %s = lshr i32 %s,16%s\n", factorBigHiIdent, factorBigIdent, cgs.debugLocation); err != nil {
				return err
			}
			prodHiIdent := cgs.ident("prod_hi")
			if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s%s\n", prodHiIdent, factorBigHiIdent, factorSmallIdent, cgs.debugLocation); err != nil {
				return err
			}
			prodHiOverflowCheckIdent := cgs.ident("prod_hi_overflow_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,65536%s\n", prodHiOverflowCheckIdent, prodHiIdent, cgs.debugLocation); err != nil {
				return err
			}
			prodHiNotOverflowLabel := cgs.label("prod_hi_not_overflow")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", prodHiOverflowCheckIdent, overflowLabel, prodHiNotOverflowLabel, cgs.debugLocation); err != nil {
				return err
			}
			prodHiShiftIdent := cgs.ident("prod_hi_shift")
			if _, err := fmt.Fprintf(w, "  %s:\n    %s = shl i32 %s,16%s\n", prodHiNotOverflowLabel, prodHiShiftIdent, prodHiIdent, cgs.debugLocation); err != nil {
				return err
			}
			factorBigLoIdent := cgs.ident("factor_big_lo")
			if _, err := fmt.Fprintf(w, "    %s = and i32 %s,65535%s\n", factorBigLoIdent, factorBigIdent, cgs.debugLocation); err != nil {
				return err
			}
			prodLoIdent := cgs.ident("prod_lo")
			if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s%s\n", prodLoIdent, factorBigLoIdent, factorSmallIdent, cgs.debugLocation); err != nil {
				return err
			}
			prodBigIdent := cgs.ident("prod_big")
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,%s%s\n", prodBigIdent, prodHiShiftIdent, prodLoIdent, cgs.debugLocation); err != nil {
				return err
			}
			prodBigOverflowCheckIdent := cgs.ident("prod_big_overflow_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp ult i32 %s,%s%s\n", prodBigOverflowCheckIdent, prodBigIdent, prodLoIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", prodBigOverflowCheckIdent, overflowLabel, v03getsLabel, cgs.debugLocation); err != nil {
				return err
			}

			if _, err := fmt.Fprintf(w, "  %s:\n", v03getsLabel); err != nil {
				return err
			}
			if !cgs.varInfo(Var32(1)).accessed {
				if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", doneLabel, cgs.debugLocation); err != nil {
					return err
				}
			} else {
				prodPhiIdent := cgs.ident("prod_phi")
				if _, err := fmt.Fprintf(w, "    %s = phi i32 [%s,%%%s],[%s,%%%s]%s\n", prodPhiIdent, prodSmallIdent, mulSmallLabel, prodBigIdent, prodHiNotOverflowLabel, cgs.debugLocation); err != nil {
					return err
				}
				prodValIdent := cgs.ident("prod_val")
				if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val insertvalue(%%val zeroinitializer,i2 1,0),i32 %s,1%s\n", prodValIdent, prodPhiIdent, cgs.debugLocation); err != nil {
					return err
				}
				if err := cgs.genGets(w, Var32(3), prodValIdent, doneLabel); err != nil {
					return err
				}
			}

			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 1999,1),i32 %d)%s\n    br label %%%s%s\n", overflowLabel, cgs.nextIndex(), cgs.debugLocation, doneLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
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
			v1 := state.Var16(Var16(1))
			if v1.Ignored {
				return nil
			}
			v1.Value = 1 + uint32(state.Random.Intn(65535))
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			if !cgs.varInfo(Var16(1)).accessed {
				return nil
			}
			doneLabel := cgs.label("lib1900_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(1)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1900_spot1_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			randomIdent := cgs.ident("random")
			if _, err := fmt.Fprintf(w, "    %s = call i32 @lib1900()%s\n", randomIdent, cgs.debugLocation); err != nil {
				return err
			}
			randomValIdent := cgs.ident("random_val")
			if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val zeroinitializer,i32 %s,1%s\n", randomValIdent, randomIdent, cgs.debugLocation); err != nil {
				return err
			}
			if err := cgs.genGets(w, Var16(1), randomValIdent, doneLabel); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
	1910: LibraryFunction{
		// .2 <- normal random no. from #0 to .1, with
		// standard deviation .1 divided by #12
		Interp: func(state *State) *Error {
			v2 := state.Var16(Var16(2))
			if v2.Ignored {
				return nil
			}
			v1 := state.Var16(Var16(1))
			if v1.Value == 0 {
				v2.Value = 0
			} else {
				r := state.Random.NormFloat64()/12*float64(v1.Value) + float64(v1.Value)/2
				if r < 0 {
					r = 0
				} else if r > float64(v1.Value) {
					r = float64(v1.Value)
				}
				v2.Value = uint32(r)
			}
			return nil
		},
		CollectVarInfo: func(cgs *codeGenState) {
			cgs.collectStmtVarInfo(Var16(1), StatementLibrary).accessed = true
			cgs.collectStmtVarInfo(Var16(2), StatementLibrary).assigned = true
		},
		CodeGen: func(w io.Writer, cgs *codeGenState) error {
			if !cgs.varInfo(Var16(2)).accessed {
				return nil
			}
			doneLabel := cgs.label("lib1910_resume")
			if ignoredIdent, err := cgs.genCheckIgnored(w, Var16(2)); err != nil {
				return err
			} else {
				notIgnoredLabel := cgs.label("lib1910_spot2_not_ignored")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notIgnoredLabel, cgs.debugLocation, notIgnoredLabel); err != nil {
					return err
				}
			}
			v1ValIdent := cgs.ident("spot1_val")
			if err := cgs.genAccessVar(w, v1ValIdent, cgs.varInfo(Var16(1))); err != nil {
				return err
			}
			v1Ident := cgs.ident("spot1_")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", v1Ident, v1ValIdent, cgs.debugLocation); err != nil {
				return err
			}
			randomIdent := cgs.ident("random")
			if _, err := fmt.Fprintf(w, "    %s = call i32 @lib1910(i32 %s)%s\n", randomIdent, v1Ident, cgs.debugLocation); err != nil {
				return err
			}
			randomValIdent := cgs.ident("random_val")
			if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val zeroinitializer,i32 %s,1%s\n", randomValIdent, randomIdent, cgs.debugLocation); err != nil {
				return err
			}
			if err := cgs.genGets(w, Var16(2), randomValIdent, doneLabel); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
				return err
			}
			return nil
		},
	},
}
