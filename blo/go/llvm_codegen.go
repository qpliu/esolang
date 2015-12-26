package main

// This is written for LLVM 3.7.0

import (
	"bytes"
	"fmt"
	"io"
	"unicode"
)

func LLVMRefcountType(ast *Ast) string {
	switch {
	case ast.MaxLocalRefs < 256:
		return "i8"
	case ast.MaxLocalRefs < 65536:
		return "i16"
	default:
		return "i32"
	}
}

func LLVMOffsetType(ast *Ast) string {
	switch {
	case ast.MaxBitIndex < 256:
		return "i8"
	case ast.MaxBitIndex < 65536:
		return "i16"
	default:
		return "i32"
	}
}

func LLVMCanonicalName(name string) string {
	var buf bytes.Buffer
	for _, rune := range name {
		if rune <= unicode.MaxASCII && (unicode.IsDigit(rune) || unicode.IsLetter(rune)) {
			buf.WriteRune(rune)
		} else {
			buf.WriteString(fmt.Sprintf("_%x_", rune))
		}
	}
	return buf.String()
}

type LLVMStmtAnnotation struct {
	startBlock        bool
	blockLabel        int
	comesFrom         []Stmt
	locals            map[string]int
	oldLocalForAssign int
	allocas           []int
	allocaType        *Type
}

type LLVMExprAnnotation struct {
	allocas    []int
	allocaType *Type
}

func LLVMCodeGen(ast *Ast, w io.Writer) error {
	if err := AnnotateRuntimeLLVM(ast); err != nil {
		return err
	}
	for _, funcDecl := range ast.Funcs {
		LLVMCodeGenAnnotateFunc(ast, funcDecl)
	}
	if err := LLVMCodeGenPrologue(ast, w); err != nil {
		return err
	}
	for _, funcDecl := range ast.Funcs {
		if err := LLVMCodeGenFunc(ast, funcDecl, w); err != nil {
			return err
		}
	}
	return nil
}

func LLVMCodeGenPrologue(ast *Ast, w io.Writer) error {
	refCountType := LLVMRefcountType(ast)
	offsetType := LLVMOffsetType(ast)
	if _, err := io.WriteString(w, fmt.Sprintf("define void @__clear({%s, [0 x i1]}* %%v, %s %%bitsize) { 0: %%1 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%v, i32 0, i32 0 store %s 0, %s* %%1 br label %%2 2: %%3 = phi %s [0, %%0], [%%7, %%5] %%4 = icmp lt %s %%3, %%bitsize br i1 %%4, label %%5, label %%8 5: %%6 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%v, i32 0, i32 1, %s %%3 store i1 0, i1* %%6 %%7 = add %s, %%3, 1 br label %%2 8: ret void }", refCountType, offsetType, refCountType, refCountType, offsetType, offsetType, offsetType, offsetType, refCountType, refCountType, offsetType, offsetType)); err != nil {
		return err
	}
	if _, err := io.WriteString(w, fmt.Sprintf("define void @__ref({%s, [0 x i1]}* %%v) { %%0 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%v, i32 0, i32 0 %%1 = load %s* %%0 %%2 = add %s %%1, 1 store %s %%2, %s* %%0 ret void }", refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, refCountType)); err != nil {
		return err
	}
	if _, err := io.WriteString(w, fmt.Sprintf("define void @__unref({%s, [0 x i1]}* %%v) { %%0 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%v, i32 0, i32 0 %%1 = load %s* %%0 %%2 = sub %s %%1, 1 store %s %%2, %s* %%0 ret void }", refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, refCountType)); err != nil {
		return err
	}
	if _, err := io.WriteString(w, fmt.Sprintf("define void @__copy({%s, [0 x i1]}* %%srcval, %s %%srcoffset, {%s, [0 x i1]}* %%destval, %s %%destoffset, %s %%bitsize) { 0: br label %%1 1: %%2 = phi %s [0, %%0], [%%10, %%4] %%3 = icmp lt %s %%2, %%bitsize br i1 %%3, label %%4, label %%11 4: %%5 = add %s %%2, %%srcoffset %%6 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%srcval, i32 0, i32 1, %s %%5 %%7 = load i1* %%6 %%8 = add %s %%2, %%destoffset %%9 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%destval, i32 0, i32 1, %s %%8 store i1 %%7, i1* %%9 %%10 = add %s %%2, 1 br label %%1 11: ret void }", refCountType, offsetType, refCountType, offsetType, offsetType, offsetType, offsetType, offsetType, refCountType, refCountType, offsetType, offsetType, refCountType, refCountType, offsetType, offsetType)); err != nil {
		return err
	}
	for i := 2; i <= ast.MaxLocalRefs; i++ {
		if _, err := io.WriteString(w, fmt.Sprintf("define {%s, [0 x i1]}* @__alloc%d(%s %%bitsize", refCountType, i, offsetType)); err != nil {
			return err
		}
		for j := 0; j < i; j++ {
			if _, err := io.WriteString(w, fmt.Sprintf(",{%s, [0 x i1]}* %%a%d", refCountType, j)); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, fmt.Sprintf(") {")); err != nil {
			return err
		}
		v := 0
		for j := 0; j < i; j++ {
			if _, err := io.WriteString(w, fmt.Sprintf(" l%d: %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%a%d, i32 0, i32 0 %%%d = load %s, %s* %%%d %%%d = icmp eq %s %%%d, 0 br i1 %%%d, label %%l%d, label %%l%d", j, v, refCountType, refCountType, j, v+1, refCountType, refCountType, v, v+2, refCountType, v+1, v+2, i+1, j+1)); err != nil {
				return err
			}
			v += 3
		}
		if _, err := io.WriteString(w, fmt.Sprintf(" l%d: ret {%s, [0 x i1]}* null ; panic - this should not happen\nl%d: %%%d = phi {%s, [0 x i1]}* [%%a0, %%l0]", i, refCountType, i+1, v, refCountType)); err != nil {
			return err
		}
		for j := 1; j < i; j++ {
			if _, err := io.WriteString(w, fmt.Sprintf(", [%%a%d, %%l%d]", j, j)); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, fmt.Sprintf(" call void @__clear({%s, [0 x i1]}* %%%v, %%bitsize) ret {%s, [0 x i1]}* %%%d }", refCountType, v, refCountType, v)); err != nil {
			return err
		}
	}
	return nil
}

func LLVMCodeGenAnnotateFunc(ast *Ast, funcDecl *Func) {
	if funcDecl.Imported {
		return
	}
	if len(funcDecl.Body.Stmts) > 0 {
		funcDecl.Body.Stmts[0].LLVMAnnotation().startBlock = true
	}
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		switch st := stmt.(type) {
		case *StmtIf:
			st.LLVMAnnotation().startBlock = true
			st.Stmts.LLVMAnnotation().comesFrom = append(st.Stmts.LLVMAnnotation().comesFrom, stmt)
			if st.ElseIf != nil {
				st.ElseIf.LLVMAnnotation().startBlock = true
				st.ElseIf.LLVMAnnotation().comesFrom = append(st.ElseIf.LLVMAnnotation().comesFrom, stmt)
			}
			if st.Else != nil {
				st.Else.LLVMAnnotation().startBlock = true
				st.Else.LLVMAnnotation().comesFrom = append(st.Else.LLVMAnnotation().comesFrom, stmt)
			}
			if st.Next != nil {
				st.Next.LLVMAnnotation().startBlock = true
			}
		case *StmtFor:
			st.LLVMAnnotation().startBlock = true
		case *StmtBreak:
			if st.Next != nil {
				st.Next.LLVMAnnotation().startBlock = true
				st.Next.LLVMAnnotation().comesFrom = append(st.Next.LLVMAnnotation().comesFrom, stmt)
			}
		}
		return nil
	})
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		switch st := stmt.(type) {
		case *StmtVar:
			if st.Next != nil && st.Next.LLVMAnnotation().startBlock {
				st.Next.LLVMAnnotation().comesFrom = append(st.Next.LLVMAnnotation().comesFrom, stmt)
			}
		case *StmtSetClear:
			if st.Next != nil && st.Next.LLVMAnnotation().startBlock {
				st.Next.LLVMAnnotation().comesFrom = append(st.Next.LLVMAnnotation().comesFrom, stmt)
			}
		case *StmtAssign:
			if st.Next != nil && st.Next.LLVMAnnotation().startBlock {
				st.Next.LLVMAnnotation().comesFrom = append(st.Next.LLVMAnnotation().comesFrom, stmt)
			}
		case *StmtExpr:
			if st.Next != nil && st.Next.LLVMAnnotation().startBlock {
				st.Next.LLVMAnnotation().comesFrom = append(st.Next.LLVMAnnotation().comesFrom, stmt)
			}
		}
		return nil
	})
	blockLabel := 0
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		ann := stmt.LLVMAnnotation()
		if ann.startBlock {
			blockLabel++
		}
		ann.blockLabel = blockLabel
		return nil
	})

	local := len(funcDecl.Params)
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		ann := stmt.LLVMAnnotation()
		ann.locals = make(map[string]int)
		if len(ann.comesFrom) == 0 {
			for i, param := range funcDecl.Params {
				ann.locals[param.Name] = i
			}
		} else if len(ann.comesFrom) == 1 {
			for name, _ := range stmt.Scope() {
				if prev, ok := ann.comesFrom[0].LLVMAnnotation().locals[name]; ok {
					ann.locals[name] = prev
				} else {
					ann.locals[name] = local
					local++
				}
			}
		} else {
			if !ann.startBlock {
				panic("Block with multiple come froms needs startBlock")
			}
			for name, _ := range stmt.Scope() {
				ann.locals[name] = local
				local++
			}
		}
		switch st := stmt.(type) {
		case *StmtAssign:
			if lvalue, ok := st.LValue.(*ExprVar); ok {
				ann.oldLocalForAssign = ann.locals[lvalue.Name]
				ann.locals[lvalue.Name] = local
				local++
			}
		}
		return nil
	})

	alloca := 0
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		st, ok := stmt.(*StmtVar)
		if !ok {
			return nil
		}
		ann := stmt.LLVMAnnotation()
		ann.allocaType = st.Var.Type
		ann.allocas = append(ann.allocas, alloca)
		alloca++
		if !inLoop {
			return nil
		}
		for name, v := range st.Scope() {
			if name != st.Var.Name && st.Var.Type.Contains(v.Type) {
				ann.allocas = append(ann.allocas, alloca)
				alloca++
			}
		}
		return nil
	})
	WalkExprs(funcDecl, func(stmt Stmt, inLoop bool, expr Expr, inAssign bool) error {
		ex, ok := expr.(*ExprFunc)
		if !ok || ex.Func.Type == nil {
			return nil
		}
		ann := expr.LLVMAnnotation()
		ann.allocaType = ex.Func.Type
		ann.allocas = append(ann.allocas, alloca)
		alloca++
		if !inLoop || !inAssign {
			return nil
		}
		for _, v := range stmt.Scope() {
			if ex.Func.Type.Contains(v.Type) {
				ann.allocas = append(ann.allocas, alloca)
				alloca++
			}
		}
		return nil
	})
}

func LLVMCodeGenFunc(ast *Ast, funcDecl *Func, w io.Writer) error {
	refCountType := LLVMRefcountType(ast)
	offsetType := LLVMOffsetType(ast)
	if funcDecl.Imported {
		return funcDecl.RuntimeLLVM(ast, funcDecl, w)
	}
	if funcDecl.Type == nil {
		if _, err := io.WriteString(w, "define void "); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, fmt.Sprintf("define {{%s, [0 x i1]}*, %s} ", refCountType, offsetType)); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, fmt.Sprintf("@%s(", LLVMCanonicalName(funcDecl.Name))); err != nil {
		return err
	}
	for i, _ := range funcDecl.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ","); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, fmt.Sprintf("{%s, [0 x i1]}* %%value%d,%s %%offset%d", refCountType, i, offsetType, i)); err != nil {
			return err
		}
	}
	if funcDecl.Type != nil {
		if len(funcDecl.Params) > 0 {
			if _, err := io.WriteString(w, ","); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, fmt.Sprintf("{%s, [0 x i1]}* %%retval", refCountType)); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") { entry:"); err != nil {
		return err
	}
	ssaName := 0
	for _, typeDecl := range ast.Types {
		if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* null, i32 0, i32 1, %s %d %%sizeof.%s = ptrtoint i1* %%%d to %s", ssaName, refCountType, refCountType, offsetType, typeDecl.BitSize(), LLVMCanonicalName(typeDecl.Name), ssaName, offsetType)); err != nil {
			return err
		}
		ssaName += 1
	}
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		ann := stmt.LLVMAnnotation()
		for _, alloca := range ann.allocas {
			if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = alloca i8, %s %%sizeof.%s %%alloca%d = bitcast i8* %d to {%s, [0 x i1]}*", ssaName, offsetType, LLVMCanonicalName(ann.allocaType.Name), alloca, ssaName, refCountType)); err != nil {
				return err
			}
			ssaName += 1
		}
		return nil
	})
	WalkExprs(funcDecl, func(stmt Stmt, inLoop bool, expr Expr, inAssign bool) error {
		ann := expr.LLVMAnnotation()
		for _, alloca := range ann.allocas {
			if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = alloca i8, %s %%sizeof.%s %%alloca%d = bitcast i8* %d to {%s, [0 x i1]}*", ssaName, offsetType, LLVMCanonicalName(ann.allocaType.Name), alloca, ssaName, refCountType)); err != nil {
				return err
			}
			ssaName += 1
		}
		return nil
	})
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		ann := stmt.LLVMAnnotation()
		for _, alloca := range ann.allocas {
			if _, err := io.WriteString(w, fmt.Sprintf(" call void @__clear({%s, [0 x i1]}* %%alloca%d, %s %d)", refCountType, alloca, offsetType, ann.allocaType.BitSize())); err != nil {
				return err
			}
		}
		return nil
	})
	WalkExprs(funcDecl, func(stmt Stmt, inLoop bool, expr Expr, inAssign bool) error {
		ann := expr.LLVMAnnotation()
		for _, alloca := range ann.allocas {
			if _, err := io.WriteString(w, fmt.Sprintf(" call void @__clear({%s, [0 x i1]}* %%alloca%d, %s %d)", refCountType, alloca, offsetType, ann.allocaType.BitSize())); err != nil {
				return err
			}
		}
		return nil
	})
	for i, _ := range funcDecl.Params {
		if _, err := io.WriteString(w, fmt.Sprintf(" call void @__ref({%s, [0 x i1]}* %%value%d)", refCountType, i)); err != nil {
			return err
		}
	}
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		ann := stmt.LLVMAnnotation()
		if ann.startBlock {
			if _, err := io.WriteString(w, fmt.Sprintf(" block%d:", ann.blockLabel)); err != nil {
				return err
			}
			//... phis
		}
		writeUnrefs := func(next Stmt) error {
			//...
			return nil
		}
		writeGotoNext := func(next Stmt) error {
			if next == nil {
				if funcDecl.Type != nil {
					panic("Cannot fall off end of func without return")
				}
				_, err := io.WriteString(w, " ret void")
				return err
			}
			if next.LLVMAnnotation().startBlock {
				_, err := io.WriteString(w, fmt.Sprintf(" br label %%block%d", next.LLVMAnnotation().blockLabel))
				return err
			}
			return nil
		}
		switch st := stmt.(type) {
		case *StmtBlock:
			if len(st.Stmts) == 0 {
				if err := writeUnrefs(st.Next); err != nil {
					return err
				}
				if err := writeGotoNext(st.Next); err != nil {
					return err
				}
			} else {
				if err := writeGotoNext(st.Stmts[0]); err != nil {
					return err
				}
			}
		case *StmtVar:
			io.WriteString(w, " ; var\n")
			//...
			if err := writeUnrefs(st.Next); err != nil {
				return err
			}
			if err := writeGotoNext(st.Next); err != nil {
				return err
			}
		case *StmtIf:
			io.WriteString(w, " ; if\n")
			//...
		case *StmtFor:
		case StmtContinue:
			// unrefs should be taken care of by previous stmt
			if _, err := io.WriteString(w, fmt.Sprintf(" br label %%block%d", st.Next.LLVMAnnotation().blockLabel)); err != nil {
				return err
			}
		case *StmtBreak:
			if err := writeUnrefs(st.Next); err != nil {
				return err
			}
			if err := writeGotoNext(st.Next); err != nil {
				return err
			}
		case *StmtReturn:
			io.WriteString(w, " ; return\n")
			if err := writeUnrefs(nil); err != nil {
				return err
			}
			//...
		case *StmtSetClear:
			io.WriteString(w, " ; set/clear\n")
			//...
			if err := writeUnrefs(st.Next); err != nil {
				return err
			}
			if err := writeGotoNext(st.Next); err != nil {
				return err
			}
		case *StmtAssign:
			io.WriteString(w, " ; assign\n")
			//...
			if err := writeUnrefs(st.Next); err != nil {
				return err
			}
			if err := writeGotoNext(st.Next); err != nil {
				return err
			}
		case *StmtExpr:
			io.WriteString(w, " ; expr\n")
			//...
			if err := writeUnrefs(st.Next); err != nil {
				return err
			}
			if err := writeGotoNext(st.Next); err != nil {
				return err
			}
		default:
			panic("Unknown stmt")
		}
		return nil
	})
	if _, err := io.WriteString(w, " }"); err != nil {
		return err
	}
	return nil
}
