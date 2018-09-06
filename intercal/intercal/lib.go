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
