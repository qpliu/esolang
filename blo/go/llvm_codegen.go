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
	for _, funcDecl := range ast.Funcs {
		if err := LLVMCodeGenFunc(ast, funcDecl, w); err != nil {
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
