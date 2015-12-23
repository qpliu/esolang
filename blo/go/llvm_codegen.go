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
	breakTarget := make(map[Stmt]bool)
	WalkStmts(funcDecl.Body, func(stmt Stmt) {
		if st, ok := stmt.(*StmtBreak); ok {
			breakTarget[st.Next] = true
		}
	})
	i := 1
	label := make(map[Stmt]int)
	WalkStmts(funcDecl.Body, func(stmt Stmt) {
		if breakTarget[stmt] {
			if _, ok := label[stmt]; !ok {
				label[stmt] = i
				i++
			}
		}
		switch st := stmt.(type) {
		case *StmtIf:
			label[st.Stmts] = i
			i++
			if st.ElseIf != nil {
				label[st.ElseIf] = i
				i++
			}
			if st.Else != nil {
				label[st.Else] = i
				i++
			}
			if st.Next != nil {
				if _, ok := label[st.Next]; !ok {
					label[st.Next] = i
					i++
				}
			}
		case *StmtFor:
			if _, ok := label[stmt]; !ok {
				label[stmt] = i
				i++
			}
		}
	})
	//...
	return nil
}
