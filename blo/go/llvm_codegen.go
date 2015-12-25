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
	startBlock bool
	blockLabel int
	comesFrom  []Stmt
	locals     map[string]int
	allocas    []int
}

type LLVMExprAnnotation struct {
	allocas []int
}

func LLVMCodeGen(ast *Ast, w io.Writer) error {
	if err := AnnotateRuntimeLLVM(ast); err != nil {
		return err
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

func LLVMCodeGenFunc(ast *Ast, funcDecl *Func, w io.Writer) error {
	refCountType := LLVMRefcountType(ast)
	offsetType := LLVMOffsetType(ast)
	if funcDecl.Imported {
		return funcDecl.RuntimeLLVM(ast, funcDecl, w)
	}
	WalkStmts(funcDecl.Body, func(stmt Stmt, inLoop bool) {
		stmt.LLVMAnnotation().locals = make(map[string]int)
		switch st := stmt.(type) {
		case *StmtIf:
			st.Stmts.LLVMAnnotation().startBlock = true
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
	})
	WalkStmts(funcDecl.Body, func(stmt Stmt, inLoop bool) {
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
	})
	blockLabel := 0
	WalkStmts(funcDecl.Body, func(stmt Stmt, inLoop bool) {
		ann := stmt.LLVMAnnotation()
		if ann.startBlock {
			blockLabel++
		}
		ann.blockLabel = blockLabel
	})
	//...
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
	for i, param := range funcDecl.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ","); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, fmt.Sprintf("{%s, [0 x i1]}* %%pv.%s,%s %%po.%s", refCountType, LLVMCanonicalName(param.Name), offsetType, LLVMCanonicalName(param.Name))); err != nil {
			return err
		}
	}
	if funcDecl.Type != nil {
		if len(funcDecl.Params) > 0 {
			if _, err := io.WriteString(w, ","); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, fmt.Sprintf("{%s, [0 x i1]}* %%rv", refCountType)); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") {"); err != nil {
		return err
	}
	//...
	if _, err := io.WriteString(w, "}"); err != nil {
		return err
	}
	return nil
}
