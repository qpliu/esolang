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
			buf.WriteString(fmt.Sprintf("_%x", rune))
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
	if _, err := io.WriteString(w, fmt.Sprintf("declare void @llvm.memset.p0i8.%s(i8*,i8,%s,i32,i1)", offsetType, offsetType)); err != nil {
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
		if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%%d, i32 0, i32 1, %s 0 %%%d = bitcast i1* %%%d to i8* %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* null, i32 0, i32 1, %s 0 %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* null, i32 0, i32 1, %s %%bitsize %%%d = ptrtoint i1* %%%d to %s %%%d = ptrtoint i1* %%%d to %s %%%d = sub %s %%%d, %%%d call void @llvm.memset.p0i8.%s(i8* %%%d,i8 0, %s %%%d, i32 0, i1 0) ret {%s, [0 x i1]}* %%%d }", v+1, refCountType, refCountType, v, offsetType, v+2, v+1, v+3, offsetType, refCountType, offsetType, v+4, refCountType, refCountType, offsetType, v+5, v+3, offsetType, v+6, v+4, offsetType, v+7, offsetType, v+6, v+5, offsetType, v+2, offsetType, v+7, refCountType, v)); err != nil {
			return err
		}
	}
	return nil
}

func LLVMCodeGenFunc(ast *Ast, funcDecl *Func, w io.Writer) error {
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
	return nil
}
