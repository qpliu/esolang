package main

// This is written for LLVM 3.7.0

import (
	"bytes"
	"fmt"
	"io"
	"sort"
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
			fmt.Fprintf(&buf, "_%x_", rune)
		}
	}
	return buf.String()
}

type LLVMStmtAnnotation struct {
	startBlock    bool
	blockLabel    int
	comesFrom     []Stmt
	localsOnEntry map[string]int
	localsOnExit  map[string]int
	allocas       []int
	allocaType    *Type
}

type LLVMExprAnnotation struct {
	allocas    []int
	allocaType *Type
}

type LLVMImportedRuntimeType struct {
	WritePrologue func(io.Writer) error
	WriteInit     func(*int, io.Writer) (int, error)
	WriteRef      func(string, *int, io.Writer) error
	WriteUnref    func(string, *int, io.Writer) error
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
	var funcNames []string
	for funcName, _ := range ast.Funcs {
		funcNames = append(funcNames, funcName)
	}
	sort.Strings(funcNames)
	for _, funcName := range funcNames {
		if err := LLVMCodeGenFunc(ast, ast.Funcs[funcName], w); err != nil {
			return err
		}
	}
	return nil
}

func LLVMCodeGenPrologue(ast *Ast, w io.Writer) error {
	refCountType := LLVMRefcountType(ast)
	offsetType := LLVMOffsetType(ast)
	ast.LLVMDeclares[fmt.Sprintf("llvm.memset.p0i8.%s", offsetType)] = fmt.Sprintf("declare void @llvm.memset.p0i8.%s(i8*,i8,%s,i32,i1)", offsetType, offsetType)
	var declares []string
	for declare, _ := range ast.LLVMDeclares {
		declares = append(declares, declare)
	}
	sort.Strings(declares)
	for _, declare := range declares {
		if _, err := io.WriteString(w, ast.LLVMDeclares[declare]); err != nil {
			return err
		}
	}
	if _, err := fmt.Fprintf(w, "define void @__copy({%s, [0 x i1]}* %%srcval, %s %%srcoffset, {%s, [0 x i1]}* %%destval, %s %%destoffset, %s %%bitsize) { br label %%l1 l1: %%1 = phi %s [0, %%0], [%%8, %%l2] %%2 = icmp ult %s %%1, %%bitsize br i1 %%2, label %%l2, label %%l3 l2: %%3 = add %s %%1, %%srcoffset %%4 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%srcval, i32 0, i32 1, %s %%3 %%5 = load i1, i1* %%4 %%6 = add %s %%1, %%destoffset %%7 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%destval, i32 0, i32 1, %s %%6 store i1 %%5, i1* %%7 %%8 = add %s %%1, 1 br label %%l1 l3: ret void }", refCountType, offsetType, refCountType, offsetType, offsetType, offsetType, offsetType, offsetType, refCountType, refCountType, offsetType, offsetType, refCountType, refCountType, offsetType, offsetType); err != nil {
		return err
	}
	for i := 2; i <= ast.MaxLocalRefs; i++ {
		if _, err := fmt.Fprintf(w, "define {%s, [0 x i1]}* @__alloc%d(", refCountType, i); err != nil {
			return err
		}
		comma := ""
		for j := 0; j < i; j++ {
			if _, err := fmt.Fprintf(w, "%s{%s, [0 x i1]}* %%a%d", comma, refCountType, j); err != nil {
				return err
			}
			comma = ","
		}
		if _, err := fmt.Fprintf(w, ") {"); err != nil {
			return err
		}
		v := 0
		for j := 0; j < i; j++ {
			if _, err := fmt.Fprintf(w, " l%d: %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%a%d, i32 0, i32 0 %%%d = load %s, %s* %%%d %%%d = icmp eq %s %%%d, 0 br i1 %%%d, label %%l%d, label %%l%d", j, v, refCountType, refCountType, j, v+1, refCountType, refCountType, v, v+2, refCountType, v+1, v+2, i+1, j+1); err != nil {
				return err
			}
			v += 3
		}
		if _, err := fmt.Fprintf(w, " l%d: ret {%s, [0 x i1]}* null ; panic - this should not happen\nl%d: %%%d = phi {%s, [0 x i1]}* [%%a0, %%l0]", i, refCountType, i+1, v, refCountType); err != nil {
			return err
		}
		for j := 1; j < i; j++ {
			if _, err := fmt.Fprintf(w, ", [%%a%d, %%l%d]", j, j); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, " ret {%s, [0 x i1]}* %%%d }", refCountType, v); err != nil {
			return err
		}
	}
	var typeNames []string
	for typeName, _ := range ast.Types {
		typeNames = append(typeNames, typeName)
	}
	sort.Strings(typeNames)
	for _, typeName := range typeNames {
		typeDecl := ast.Types[typeName]
		if typeDecl.Imported && typeDecl.LLVMRTType.WritePrologue != nil {
			if err := typeDecl.LLVMRTType.WritePrologue(w); err != nil {
				return err
			}
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
		case *StmtBlock:
			if len(st.Stmts) > 0 {
				st.Stmts[0].LLVMAnnotation().comesFrom = append(st.Stmts[0].LLVMAnnotation().comesFrom, stmt)
			} else if st.Next != nil {
				st.Next.LLVMAnnotation().comesFrom = append(st.Next.LLVMAnnotation().comesFrom, stmt)
			}
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
			st.Stmts.LLVMAnnotation().comesFrom = append(st.Stmts.LLVMAnnotation().comesFrom, st)
		case StmtContinue:
		case *StmtBreak:
			if st.Next != nil {
				st.Next.LLVMAnnotation().startBlock = true
				st.Next.LLVMAnnotation().comesFrom = append(st.Next.LLVMAnnotation().comesFrom, stmt)
			}
		case *StmtVar:
			if st.Next != nil {
				st.Next.LLVMAnnotation().comesFrom = append(st.Next.LLVMAnnotation().comesFrom, stmt)
			}
		case *StmtSetClear:
			if st.Next != nil {
				st.Next.LLVMAnnotation().comesFrom = append(st.Next.LLVMAnnotation().comesFrom, stmt)
			}
		case *StmtAssign:
			if st.Next != nil {
				st.Next.LLVMAnnotation().comesFrom = append(st.Next.LLVMAnnotation().comesFrom, stmt)
			}
		case *StmtExpr:
			if st.Next != nil {
				st.Next.LLVMAnnotation().comesFrom = append(st.Next.LLVMAnnotation().comesFrom, stmt)
			}
		case *StmtReturn:
		default:
			panic("Unknown Stmt type")
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
		ann.localsOnEntry = make(map[string]int)
		if len(ann.comesFrom) == 0 {
			for i, param := range funcDecl.Params {
				ann.localsOnEntry[param.Name] = i
			}
		} else if len(ann.comesFrom) == 1 {
			varName := ""
			if st, ok := stmt.(*StmtVar); ok {
				varName = st.Var.Name
			}
			for name, _ := range stmt.Scope() {
				if name == varName {
				} else if prev, ok := ann.comesFrom[0].LLVMAnnotation().localsOnExit[name]; ok {
					ann.localsOnEntry[name] = prev
				} else {
					panic("Mysteriously appearing in scope:" + name)
				}
			}
		} else {
			if !ann.startBlock {
				panic("Block with multiple come froms needs startBlock")
			}
			varName := ""
			if st, ok := stmt.(*StmtVar); ok {
				varName = st.Var.Name
			}
			var names []string
			for name, _ := range stmt.Scope() {
				if name != varName {
					names = append(names, name)
				}
			}
			sort.Strings(names)
			for _, name := range names {
				ann.localsOnEntry[name] = local
				local++
			}
		}
		ann.localsOnExit = ann.localsOnEntry
		switch st := stmt.(type) {
		case *StmtVar:
			ann.localsOnExit = make(map[string]int)
			for k, v := range ann.localsOnEntry {
				ann.localsOnExit[k] = v
			}
			ann.localsOnExit[st.Var.Name] = local
			local++
		case *StmtAssign:
			ex := st.LValue
		loop:
			for {
				switch lvalue := ex.(type) {
				case *ExprVar:
					ann.localsOnExit = make(map[string]int)
					for k, v := range ann.localsOnEntry {
						ann.localsOnExit[k] = v
					}
					ann.localsOnExit[lvalue.Name] = local
					local++
					break loop
				case *ExprField:
					ex = lvalue.Expr
				case *ExprFunc:
					break loop
				default:
					panic("Unknown Expr type")
				}
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
			if name != st.Var.Name && (st.Var.Type == v.Type || st.Var.Type.Contains(v.Type)) {
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
			if ex.Func.Type == v.Type || ex.Func.Type.Contains(v.Type) {
				ann.allocas = append(ann.allocas, alloca)
				alloca++
			}
		}
		return nil
	})
}

func importedCount(typeDecl *Type) int {
	if typeDecl == nil {
		return 0
	}
	count := 0
	if typeDecl.Imported {
		count++
	}
	for _, field := range typeDecl.Fields {
		count += importedCount(field.Type)
	}
	return count
}

func importedTypes(typeDecl *Type) []LLVMImportedRuntimeType {
	var res []LLVMImportedRuntimeType
	if typeDecl == nil {
		return res
	}
	if typeDecl.Imported {
		res = append(res, typeDecl.LLVMRTType)
	}
	for _, field := range typeDecl.Fields {
		res = append(res, importedTypes(field.Type)...)
	}
	return res
}

func importedOffset(typeDecl *Type, fieldName string) int {
	offset := 0
	if typeDecl.Imported {
		offset++
	}
	for _, field := range typeDecl.Fields {
		if fieldName == field.Name {
			return offset
		}
		offset += importedCount(field.Type)
	}
	panic("Unknown field " + fieldName)
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
		if _, err := fmt.Fprintf(w, "define {{%s, [0 x i1]}*, %s", refCountType, offsetType); err != nil {
			return err
		}
		if importedCount(funcDecl.Type) > 0 {
			if _, err := fmt.Fprintf(w, ", [%d x i8*]", importedCount(funcDecl.Type)); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "} "); err != nil {
			return err
		}
	}
	if _, err := fmt.Fprintf(w, "@%s(", LLVMCanonicalName(funcDecl.Name)); err != nil {
		return err
	}
	comma := ""
	for i, param := range funcDecl.Params {
		if _, err := fmt.Fprintf(w, "%s{%s, [0 x i1]}* %%value%d,%s %%offset%d", comma, refCountType, i, offsetType, i); err != nil {
			return err
		}
		if importedCount(param.Type) > 0 {
			if _, err := fmt.Fprintf(w, ",[%d x i8*] %%import%d", importedCount(param.Type), i); err != nil {
				return err
			}
		}
		comma = ","
	}
	if funcDecl.Type != nil {
		if len(funcDecl.Params) > 0 {
			if _, err := io.WriteString(w, ","); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "{%s, [0 x i1]}* %%retval", refCountType); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") { entry:"); err != nil {
		return err
	}
	ssaTemp := 0
	var typeNames []string
	for typeName, _ := range ast.Types {
		typeNames = append(typeNames, typeName)
	}
	sort.Strings(typeNames)
	for _, typeName := range typeNames {
		if _, err := fmt.Fprintf(w, " %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* null, i32 0, i32 1, %s %d %%sizeof.%s = ptrtoint i1* %%%d to %s", ssaTemp, refCountType, refCountType, offsetType, ast.Types[typeName].BitSize(), LLVMCanonicalName(typeName), ssaTemp, offsetType); err != nil {
			return err
		}
		ssaTemp += 1
	}
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		ann := stmt.LLVMAnnotation()
		for _, alloca := range ann.allocas {
			if _, err := fmt.Fprintf(w, " %%%d = alloca i8, %s %%sizeof.%s %%alloca%d = bitcast i8* %%%d to {%s, [0 x i1]}*", ssaTemp, offsetType, LLVMCanonicalName(ann.allocaType.Name), alloca, ssaTemp, refCountType); err != nil {
				return err
			}
			ssaTemp += 1
		}
		return nil
	})
	WalkExprs(funcDecl, func(stmt Stmt, inLoop bool, expr Expr, inAssign bool) error {
		ann := expr.LLVMAnnotation()
		for _, alloca := range ann.allocas {
			if _, err := fmt.Fprintf(w, " %%%d = alloca i8, %s %%sizeof.%s %%alloca%d = bitcast i8* %%%d to {%s, [0 x i1]}*", ssaTemp, offsetType, LLVMCanonicalName(ann.allocaType.Name), alloca, ssaTemp, refCountType); err != nil {
				return err
			}
			ssaTemp += 1
		}
		return nil
	})
	writeClear := func(val string, typeDecl *Type) error {
		if _, err := fmt.Fprintf(w, " %%%d = bitcast {%s, [0 x i1]}* %s to i8* call void @llvm.memset.p0i8.%s(i8* %%%d, i8 0, %s %%sizeof.%s, i32 0, i1 0)", ssaTemp, refCountType, val, offsetType, ssaTemp, offsetType, LLVMCanonicalName(typeDecl.Name)); err != nil {
			return err
		}
		ssaTemp++
		return nil
	}
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		ann := stmt.LLVMAnnotation()
		for _, alloca := range ann.allocas {
			if err := writeClear(fmt.Sprintf("%%alloca%d", alloca), ann.allocaType); err != nil {
				return err
			}
		}
		return nil
	})
	WalkExprs(funcDecl, func(stmt Stmt, inLoop bool, expr Expr, inAssign bool) error {
		ann := expr.LLVMAnnotation()
		for _, alloca := range ann.allocas {
			if err := writeClear(fmt.Sprintf("%%alloca%d", alloca), ann.allocaType); err != nil {
				return err
			}
		}
		return nil
	})
	writeRef := func(val int, typeDecl *Type) error {
		if _, err := fmt.Fprintf(w, " %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%value%d, i32 0, i32 0 %%%d = load %s, %s* %%%d %%%d = add %s %%%d, 1 store %s %%%d, %s* %%%d", ssaTemp, refCountType, refCountType, val, ssaTemp+1, offsetType, offsetType, ssaTemp, ssaTemp+2, offsetType, ssaTemp+1, offsetType, ssaTemp+2, offsetType, ssaTemp); err != nil {
			return err
		}
		ssaTemp += 3
		if importedCount(typeDecl) > 0 {
			for i, t := range importedTypes(typeDecl) {
				if _, err := fmt.Fprintf(w, " %%%d = extractvalue [%d x i8*] %%import%d, %d", ssaTemp, importedCount(typeDecl), val, i); err != nil {
					return err
				}
				ssaTemp++
				if err := t.WriteRef(fmt.Sprintf("%%%d", ssaTemp-1), &ssaTemp, w); err != nil {
					return err
				}
			}
		}
		return nil
	}
	writeUnref := func(val int, typeDecl *Type) error {
		if _, err := fmt.Fprintf(w, " %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%value%d, i32 0, i32 0 %%%d = load %s, %s* %%%d %%%d = sub %s %%%d, 1 store %s %%%d, %s* %%%d", ssaTemp, refCountType, refCountType, val, ssaTemp+1, offsetType, offsetType, ssaTemp, ssaTemp+2, offsetType, ssaTemp+1, offsetType, ssaTemp+2, offsetType, ssaTemp); err != nil {
			return err
		}
		ssaTemp += 3
		if importedCount(typeDecl) > 0 {
			for i, t := range importedTypes(typeDecl) {
				if _, err := fmt.Fprintf(w, " %%%d = extractvalue [%d x i8*] %%import%d, %d", ssaTemp, importedCount(typeDecl), val, i); err != nil {
					return err
				}
				ssaTemp++
				if err := t.WriteUnref(fmt.Sprintf("%%%d", ssaTemp-1), &ssaTemp, w); err != nil {
					return err
				}
			}
		}
		return nil
	}
	for i, param := range funcDecl.Params {
		if err := writeRef(i, param.Type); err != nil {
			return err
		}
	}
	writeExprRef := func(val, offs, imp int, typeDecl *Type) error {
		if importedCount(typeDecl) == 0 {
			return nil
		}
		for i, t := range importedTypes(typeDecl) {
			if _, err := fmt.Fprintf(w, " %%%d = extractvalue [%d x i8*] %%%d, %d", ssaTemp, importedCount(typeDecl), imp, i); err != nil {
				return err
			}
			ssaTemp++
			if err := t.WriteRef(fmt.Sprintf("%%%d", ssaTemp-1), &ssaTemp, w); err != nil {
				return err
			}
		}
		return nil
	}
	writeExprUnref := func(val, offs, imp int, typeDecl *Type) error {
		if importedCount(typeDecl) == 0 {
			return nil
		}
		for i, t := range importedTypes(typeDecl) {
			if _, err := fmt.Fprintf(w, " %%%d = extractvalue [%d x i8*] %%%d, %d", ssaTemp, importedCount(typeDecl), imp, i); err != nil {
				return err
			}
			ssaTemp++
			if err := t.WriteUnref(fmt.Sprintf("%%%d", ssaTemp-1), &ssaTemp, w); err != nil {
				return err
			}
		}
		return nil
	}
	var writeExpr func(Stmt, Expr, bool) (int, int, int, error)
	writeExpr = func(stmt Stmt, expr Expr, inLoop bool) (int, int, int, error) {
		switch ex := expr.(type) {
		case *ExprVar:
			if _, err := fmt.Fprintf(w, " %%%d = select i1 1, {%s, [0 x i1]}* %%value%d, {%s, [0 x i1]}* null %%%d = select i1 1, %s %%offset%d, %s 0", ssaTemp, refCountType, stmt.LLVMAnnotation().localsOnEntry[ex.Name], refCountType, ssaTemp+1, offsetType, stmt.LLVMAnnotation().localsOnEntry[ex.Name], offsetType); err != nil {
				return 0, 0, 0, err
			}
			val := ssaTemp
			offs := ssaTemp + 1
			ssaTemp += 2
			imp := 0
			if importedCount(expr.Type()) > 0 {
				if _, err := fmt.Fprintf(w, " %%%d = select i1 1, [%d x i8*] %%import%d, [%d x i8*] undef", ssaTemp, importedCount(expr.Type()), stmt.LLVMAnnotation().localsOnEntry[ex.Name], importedCount(expr.Type())); err != nil {
					return 0, 0, 0, err
				}
				imp = ssaTemp
				ssaTemp++
			}
			if err := writeExprRef(val, offs, imp, expr.Type()); err != nil {
				return 0, 0, 0, nil
			}
			return val, offs, imp, nil
		case *ExprField:
			val, offs, imp, err := writeExpr(stmt, ex.Expr, inLoop)
			if err != nil {
				return 0, 0, 0, err
			}
			if _, err := fmt.Fprintf(w, " %%%d = add %s %%%d, %d", ssaTemp, offsetType, offs, ex.Expr.Type().BitOffset(ex.Name)); err != nil {
				return 0, 0, 0, err
			}
			newoffs := ssaTemp
			ssaTemp += 1
			newimp := 0
			if importedCount(expr.Type()) > 0 {
				if _, err := fmt.Fprintf(w, " %%%d = select i1 1, [%d x i8*] undef, [%d x i8*] undef", ssaTemp, importedCount(expr.Type()), importedCount(expr.Type())); err != nil {
					return 0, 0, 0, err
				}
				ssaTemp++
				for i := 0; i < importedCount(expr.Type()); i++ {
					if _, err := fmt.Fprintf(w, " %%%d = extractvalue [%d x i8*] %%%d, %d %%%d = insertvalue [%d x i8*] %%%d, i8* %%%d, %d", ssaTemp, importedCount(ex.Expr.Type()), imp, i+importedOffset(ex.Expr.Type(), ex.Name), ssaTemp+1, importedCount(expr.Type()), ssaTemp-1, ssaTemp, i); err != nil {
						return 0, 0, 0, nil
					}
					ssaTemp += 2
				}
				newimp = ssaTemp - 1
			}
			if err := writeExprRef(val, newoffs, newimp, expr.Type()); err != nil {
				return 0, 0, 0, nil
			}
			if err := writeExprUnref(val, offs, imp, ex.Expr.Type()); err != nil {
				return 0, 0, 0, nil
			}
			return val, newoffs, newimp, nil
		case *ExprFunc:
			var args [][3]int
			for _, param := range ex.Params {
				val, offs, imp, err := writeExpr(stmt, param, inLoop)
				if err != nil {
					return 0, 0, 0, err
				}
				args = append(args, [3]int{val, offs, imp})
			}
			exprAnn := expr.LLVMAnnotation()
			retVal := 0
			retValAlloc := ""
			switch len(exprAnn.allocas) {
			case 0:
				if expr.Type() != nil {
					panic("expr function")
				}
				if _, err := io.WriteString(w, " call void"); err != nil {
					return 0, 0, 0, err
				}
			case 1:
				retValAlloc = fmt.Sprintf("%%alloca%d", exprAnn.allocas[0])
				retVal = ssaTemp
				ssaTemp++
				if inLoop {
					if err := writeClear(retValAlloc, exprAnn.allocaType); err != nil {
						return 0, 0, 0, err
					}
				}
				if _, err := fmt.Fprintf(w, " %%%d = call {{%s, [0 x i1]}*, %s", retVal, refCountType, offsetType); err != nil {
					return 0, 0, 0, err
				}
				if importedCount(expr.Type()) > 0 {
					if _, err := fmt.Fprintf(w, ", [%d x i8*]", importedCount(expr.Type())); err != nil {
						return 0, 0, 0, err
					}
				}
				if _, err := io.WriteString(w, "}"); err != nil {
					return 0, 0, 0, err
				}
			default:
				retValAlloc = fmt.Sprintf("%%%d", ssaTemp)
				retVal = ssaTemp + 1
				ssaTemp += 2
				if _, err := fmt.Fprintf(w, " %s = call {%s, [0 x i1]}* @__alloc%d(", retValAlloc, refCountType, len(exprAnn.allocas)); err != nil {
					return 0, 0, 0, err
				}
				comma := ""
				for _, alloca := range exprAnn.allocas {
					if _, err := fmt.Fprintf(w, "%s{%s, [0 x i1]}* %%alloca%d", comma, refCountType, alloca); err != nil {
						return 0, 0, 0, err
					}
					comma = ","
				}
				if _, err := fmt.Fprintf(w, ")"); err != nil {
					return 0, 0, 0, err
				}
				if err := writeClear(retValAlloc, exprAnn.allocaType); err != nil {
					return 0, 0, 0, err
				}
				if _, err := fmt.Fprintf(w, "%%%d = call {{%s, [0 x i1]}*, %s", retVal, refCountType, offsetType); err != nil {
					return 0, 0, 0, err
				}
				if importedCount(expr.Type()) > 0 {
					if _, err := fmt.Fprintf(w, ", [%d x i8*]", importedCount(expr.Type())); err != nil {
						return 0, 0, 0, err
					}
				}
				if _, err := io.WriteString(w, "}"); err != nil {
					return 0, 0, 0, err
				}
			}
			if _, err := fmt.Fprintf(w, " @%s(", LLVMCanonicalName(ex.Name)); err != nil {
				return 0, 0, 0, err
			}
			comma := ""
			for i, arg := range args {
				if _, err := fmt.Fprintf(w, "%s{%s, [0 x i1]}* %%%d, %s %%%d", comma, refCountType, arg[0], offsetType, arg[1]); err != nil {
					return 0, 0, 0, err
				}
				if importedCount(ex.Params[i].Type()) > 0 {
					if _, err := fmt.Fprintf(w, ",[%d x i8*] %%%d", importedCount(ex.Params[i].Type()), arg[2]); err != nil {
						return 0, 0, 0, err
					}
				}
				comma = ","
			}
			if len(exprAnn.allocas) > 0 {
				if _, err := fmt.Fprintf(w, "%s{%s, [0 x i1]}* %s", comma, refCountType, retValAlloc); err != nil {
					return 0, 0, 0, err
				}
			}
			if _, err := io.WriteString(w, ")"); err != nil {
				return 0, 0, 0, err
			}
			for i, arg := range args {
				if err := writeExprUnref(arg[0], arg[1], arg[2], ex.Params[i].Type()); err != nil {
					return 0, 0, 0, err
				}
			}
			if len(exprAnn.allocas) == 0 {
				return 0, 0, 0, nil
			}
			val := ssaTemp
			offs := ssaTemp + 1
			ssaTemp += 2
			imp := 0
			if importedCount(expr.Type()) == 0 {
				if _, err := fmt.Fprintf(w, " %%%d = extractvalue {{%s, [0 x i1]}*, %s} %%%d, 0 %%%d = extractvalue {{%s, [0 x i1]}*, %s} %%%d, 1", val, refCountType, offsetType, retVal, offs, refCountType, offsetType, retVal); err != nil {
					return 0, 0, 0, err
				}
			} else {
				imp = ssaTemp
				ssaTemp++
				if _, err := fmt.Fprintf(w, " %%%d = extractvalue {{%s, [0 x i1]}*, %s, [%d x i8*]} %%%d, 0 %%%d = extractvalue {{%s, [0 x i1]}*, %s, [%d x i8*]} %%%d, 1 %%%d = extractvalue {{%s, [0 x i1]}*, %s, [%d x i8*]} %%%d, 2", val, refCountType, offsetType, importedCount(expr.Type()), retVal, offs, refCountType, offsetType, importedCount(expr.Type()), retVal, imp, refCountType, offsetType, importedCount(expr.Type()), retVal); err != nil {
					return 0, 0, 0, err
				}
			}
			return val, offs, imp, nil
		default:
			panic("Unknown expr type")
		}
	}
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		ann := stmt.LLVMAnnotation()
		if ann.startBlock {
			if _, err := fmt.Fprintf(w, " block%d:", ann.blockLabel); err != nil {
				return err
			}
			if len(ann.comesFrom) > 1 {
				var vars []string
				var varTypes []*Type
				for name, _ := range ann.localsOnEntry {
					vars = append(vars, name)
					varTypes = append(varTypes, stmt.Scope()[name].Type)
				}
				sort.Strings(vars)
				for i, v := range vars {
					if _, err := fmt.Fprintf(w, " %%value%d = phi {%s, [0 x i1]}* ", ann.localsOnEntry[v], refCountType); err != nil {
						return err
					}
					comma := ""
					for _, prev := range ann.comesFrom {
						if _, err := fmt.Fprintf(w, "%s[%%value%d,%%block%d]", comma, prev.LLVMAnnotation().localsOnExit[v], prev.LLVMAnnotation().blockLabel); err != nil {
							return err
						}
						comma = ","
					}
					if _, err := fmt.Fprintf(w, " %%offset%d = phi %s ", ann.localsOnEntry[v], offsetType); err != nil {
						return err
					}
					comma = ""
					for _, prev := range ann.comesFrom {
						if _, err := fmt.Fprintf(w, "%s[%%offset%d,%%block%d]", comma, prev.LLVMAnnotation().localsOnExit[v], prev.LLVMAnnotation().blockLabel); err != nil {
							return err
						}
						comma = ","
					}
					if importedCount(varTypes[i]) > 0 {
						if _, err := fmt.Fprintf(w, " %%import%d = phi [%d x i8*] ", ann.localsOnEntry[v], importedCount(varTypes[i])); err != nil {
							return err
						}
						comma = ""
						for _, prev := range ann.comesFrom {
							if _, err := fmt.Fprintf(w, "%s[%%import%d,%%block%d]", comma, prev.LLVMAnnotation().localsOnExit[v], prev.LLVMAnnotation().blockLabel); err != nil {
								return err
							}
							comma = ","
						}
					}
				}
			}
		}
		writeUnrefs := func(next Stmt) error {
			var unrefs []int
			var unrefTypes []*Type
			for name, ref := range ann.localsOnExit {
				if next != nil {
					if _, ok := next.LLVMAnnotation().localsOnEntry[name]; ok {
						continue
					}
				}
				unrefs = append(unrefs, ref)
				unrefTypes = append(unrefTypes, stmt.Scope()[name].Type)
			}
			sort.Ints(unrefs)
			for i, unref := range unrefs {
				if err := writeUnref(unref, unrefTypes[i]); err != nil {
					return err
				}
			}
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
				_, err := fmt.Fprintf(w, " br label %%block%d", next.LLVMAnnotation().blockLabel)
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
			if st.Expr != nil {
				val, offs, imp, err := writeExpr(stmt, st.Expr, inLoop)
				if err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, " %%value%d = select i1 1, {%s, [0 x i1]}* %%%d, {%s, [0 x i1]}* null %%offset%d = select i1 1, %s %%%d, %s 0", ann.localsOnExit[st.Var.Name], refCountType, val, refCountType, ann.localsOnExit[st.Var.Name], offsetType, offs, offsetType); err != nil {
					return err
				}
				if importedCount(st.Var.Type) > 0 {
					if _, err := fmt.Fprintf(w, " %%import%d = select i1 1, [%d x i8*] %%%d, [%d x i8*] undef", ann.localsOnExit[st.Var.Name], importedCount(st.Var.Type), imp, importedCount(st.Var.Type)); err != nil {
						return err
					}
				}
				if err := writeRef(ann.localsOnExit[st.Var.Name], st.Var.Type); err != nil {
					return err
				}
				if err := writeExprUnref(val, offs, imp, st.Var.Type); err != nil {
					return err
				}
			} else {
				switch len(ann.allocas) {
				case 0:
					panic("No allocations for var statement")
				case 1:
					if _, err := fmt.Fprintf(w, " %%value%d = select i1 1, {%s, [0 x i1]}* %%alloca%d, {%s, [0 x i1]}* null %%offset%d = select i1 1, %s 0, %s 0", ann.localsOnExit[st.Var.Name], refCountType, ann.allocas[0], refCountType, ann.localsOnExit[st.Var.Name], offsetType, offsetType); err != nil {
						return err
					}
					if inLoop {
						if err := writeClear(fmt.Sprintf("%%value%d", ann.localsOnExit[st.Var.Name]), st.Var.Type); err != nil {
							return err
						}
					}
				default:
					if _, err := fmt.Fprintf(w, " %%value%d = call {%s, [0 x i1]}* @__alloc%d(", ann.localsOnExit[st.Var.Name], refCountType, len(ann.allocas)); err != nil {
						return err
					}
					comma := ""
					for _, alloca := range ann.allocas {
						if _, err := fmt.Fprintf(w, "%s{%s, [0 x i1]}* %%alloca%d", comma, refCountType, alloca); err != nil {
							return err
						}
						comma = ","
					}
					if _, err := fmt.Fprintf(w, ")"); err != nil {
						return err
					}
					if err := writeClear(fmt.Sprintf("%%value%d", ann.localsOnExit[st.Var.Name]), st.Var.Type); err != nil {
						return err
					}
					if _, err := fmt.Fprintf(w, " %%offset%d = select i1 1, %s 0, %s 0", ann.localsOnExit[st.Var.Name], offsetType, offsetType); err != nil {
						return err
					}
				}
				if importedCount(st.Var.Type) > 0 {
					if _, err := fmt.Fprintf(w, " %%%d = select i1 1, [%d x i8*] undef, [%d x i8*] undef", ssaTemp, importedCount(st.Var.Type), importedCount(st.Var.Type)); err != nil {
						return err
					}
					ssaTemp++
					for i, rttype := range importedTypes(st.Var.Type) {
						lastImp := ssaTemp - 1
						imp, err := rttype.WriteInit(&ssaTemp, w)
						if err != nil {
							return err
						}
						if _, err := fmt.Fprintf(w, " %%%d = insertvalue [%d x i8*] %%%d, i8* %%%d, %d", ssaTemp, importedCount(st.Var.Type), lastImp, imp, i); err != nil {
							return err
						}
						ssaTemp++
					}
					if _, err := fmt.Fprintf(w, " %%import%d = select i1 1, [%d x i8*] %%%d, [%d x i8*] undef", ann.localsOnExit[st.Var.Name], importedCount(st.Var.Type), ssaTemp-1, importedCount(st.Var.Type)); err != nil {
						return err
					}
				}
				if err := writeRef(ann.localsOnExit[st.Var.Name], st.Var.Type); err != nil {
					return err
				}
			}
			if err := writeUnrefs(st.Next); err != nil {
				return err
			}
			if err := writeGotoNext(st.Next); err != nil {
				return err
			}
		case *StmtIf:
			val, offs, imp, err := writeExpr(stmt, st.Expr, inLoop)
			if err != nil {
				return err
			}
			if err := writeExprUnref(val, offs, imp, st.Expr.Type()); err != nil {
				return err
			}
			addr := ssaTemp
			cond := ssaTemp + 1
			ssaTemp += 2
			var elseBlock string
			writeElseReturn := false
			if st.ElseIf != nil {
				elseBlock = fmt.Sprintf("%%block%d", st.ElseIf.LLVMAnnotation().blockLabel)
			} else if st.Else != nil {
				elseBlock = fmt.Sprintf("%%block%d", st.Else.LLVMAnnotation().blockLabel)
			} else if st.Next != nil {
				elseBlock = fmt.Sprintf("%%block%d", st.Next.LLVMAnnotation().blockLabel)
			} else {
				elseBlock = fmt.Sprintf("%%block%d.0", ann.blockLabel)
				writeElseReturn = true
				if funcDecl.Type != nil {
					panic("no return statement")
				}
			}
			if _, err := fmt.Fprintf(w, " %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%%d, i32 0, i32 1, %s %%%d %%%d = load i1, i1* %%%d br i1 %%%d, label %%block%d, label %s", addr, refCountType, refCountType, val, offsetType, offs, cond, addr, cond, st.Stmts.LLVMAnnotation().blockLabel, elseBlock); err != nil {
				return err
			}
			if writeElseReturn {
				if _, err := fmt.Fprintf(w, " block%d.0:", ann.blockLabel); err != nil {
					return err
				}
				if err := writeUnrefs(nil); err != nil {
					return err
				}
				if _, err := io.WriteString(w, " ret void"); err != nil {
					return err
				}
			}
		case *StmtFor:
		case StmtContinue:
			// unrefs should be taken care of by previous stmt
			if _, err := fmt.Fprintf(w, " br label %%block%d", st.Next.LLVMAnnotation().blockLabel); err != nil {
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
			if st.Expr == nil {
				if err := writeUnrefs(nil); err != nil {
					return err
				}
				if _, err := io.WriteString(w, " ret void"); err != nil {
					return err
				}
			} else {
				val, offs, imp, err := writeExpr(stmt, st.Expr, inLoop)
				if err != nil {
					return err
				}
				if err := writeUnrefs(nil); err != nil {
					return err
				}
				retType := fmt.Sprintf("{{%s, [0 x i1]}*, %s}", refCountType, offsetType)
				if importedCount(st.Expr.Type()) > 0 {
					retType = fmt.Sprintf("{{%s, [0 x i1]}*, %s, [%d x i8*]}", refCountType, offsetType, importedCount(st.Expr.Type()))
				}
				subblockLabel := 0
				for i, param := range funcDecl.Params {
					if param.Type != st.Expr.Type() && !param.Type.Contains(st.Expr.Type()) {
						continue
					}
					if _, err := fmt.Fprintf(w, " %%%d = icmp eq {%s, [0 x i1]}* %%%d, %%value%d br i1 %%%d, label %%block%d.%d, label %%block%d.%d block%d.%d: %%%d = insertvalue %s undef, {%s, [0 x i1]}* %%%d, 0 %%%d = insertvalue %s %%%d, %s %%%d, 1", ssaTemp, refCountType, val, i, ssaTemp, ann.blockLabel, subblockLabel, ann.blockLabel, subblockLabel+1, ann.blockLabel, subblockLabel, ssaTemp+1, retType, refCountType, val, ssaTemp+2, retType, ssaTemp+1, offsetType, offs); err != nil {
						return err
					}
					ssaTemp += 3
					if importedCount(st.Expr.Type()) > 0 {
						if _, err := fmt.Fprintf(w, " %%%d = insertvalue %s %%%d, [%d x i8*] %%%d, 2", ssaTemp, retType, ssaTemp-1, importedCount(st.Expr.Type()), imp); err != nil {
							return err
						}
					}
					if _, err := fmt.Fprintf(w, " ret %s %%%d block%d.%d:", retType, ssaTemp-1, ann.blockLabel, subblockLabel+1); err != nil {
						return err
					}
					subblockLabel += 2
				}
				if _, err := fmt.Fprintf(w, " call void @__copy({%s, [0 x i1]}* %%%d, %s %%%d, {%s, [0 x i1]}* %%retval, %s 0, %s %d) %%%d = insertvalue %s undef, {%s, [0 x i1]}* %%retval, 0 %%%d = insertvalue %s %%%d, %s 0, 1", refCountType, val, offsetType, offs, refCountType, offsetType, offsetType, st.Expr.Type().BitSize(), ssaTemp, retType, refCountType, ssaTemp+1, retType, ssaTemp, offsetType); err != nil {
					return err
				}
				ssaTemp += 2
				if importedCount(st.Expr.Type()) > 0 {
					if _, err := fmt.Fprintf(w, " %%%d = insertvalue %s %%%d, [%d x i8*] %%%d, 2", ssaTemp, retType, ssaTemp-1, importedCount(st.Expr.Type()), imp); err != nil {
						return err
					}
					ssaTemp++
				}
				if _, err := fmt.Fprintf(w, " ret %s %%%d", retType, ssaTemp-1); err != nil {
					return err
				}
			}
		case *StmtSetClear:
			val, offs, imp, err := writeExpr(stmt, st.Expr, inLoop)
			if err != nil {
				return err
			}
			if err := writeExprUnref(val, offs, imp, st.Expr.Type()); err != nil {
				return err
			}
			addr := ssaTemp
			ssaTemp++
			bit := 0
			if st.Value {
				bit = 1
			}
			if _, err := fmt.Fprintf(w, " %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%%d, i32 0, i32 1, %s %%%d store i1 %d, i1* %%%d", addr, refCountType, refCountType, val, offsetType, offs, bit, addr); err != nil {
				return err
			}
			if err := writeUnrefs(st.Next); err != nil {
				return err
			}
			if err := writeGotoNext(st.Next); err != nil {
				return err
			}
		case *StmtAssign:
			lval, loffs, limp, lerr := writeExpr(stmt, st.LValue, inLoop)
			if lerr != nil {
				return lerr
			}
			ex := st.LValue
			limpOffset := 0
		loop:
			for {
				switch lvalue := ex.(type) {
				case *ExprField:
					limpOffset += importedOffset(lvalue.Expr.Type(), lvalue.Name)
				case *ExprVar:
					val, offs, imp, err := writeExpr(stmt, st.Expr, inLoop)
					if err != nil {
						return err
					}
					if lvalue.Var.Type == st.Expr.Type() {
						if _, err := fmt.Fprintf(w, " %%value%d = select i1 1, {%s, [0 x i1]}* %%%d, {%s, [0 x i1]}* null %%offset%d = select i1 1, %s %%%d, %s 0", ann.localsOnExit[lvalue.Var.Name], refCountType, val, refCountType, ann.localsOnExit[lvalue.Var.Name], offsetType, offs, offsetType); err != nil {
							return err
						}
					} else {
						if _, err := fmt.Fprintf(w, " call void @__copy({%s, [0 x i1]}* %%%d, %s %d, {%s, [0 x i1]}* %%value%d, %s %d, %s %d)", refCountType, val, offsetType, offs, refCountType, ann.localsOnEntry[lvalue.Var.Name], offsetType, loffs, offsetType, st.Expr.Type().BitSize()); err != nil {
							return err
						}
						if _, err := fmt.Fprintf(w, " %%value%d = select i1 1, {%s, [0 x i1]}* %%value%d, {%s, [0 x i1]}* null %%offset%d = select i1 1, %s %%offset%d, %s 0", ann.localsOnExit[lvalue.Var.Name], refCountType, ann.localsOnEntry[lvalue.Var.Name], refCountType, ann.localsOnExit[lvalue.Var.Name], offsetType, ann.localsOnEntry[lvalue.Var.Name], offsetType); err != nil {
							return err
						}
					}
					if importedCount(lvalue.Var.Type) > 0 {
						if _, err := fmt.Fprintf(w, " %%%d = select i1 1, [%d x i8*] %%import%d, [%d x i8*] undef", ssaTemp, importedCount(lvalue.Var.Type), ann.localsOnEntry[lvalue.Var.Name], importedCount(lvalue.Var.Type)); err != nil {
							return err
						}
						ssaTemp++
						for i := 0; i < importedCount(st.Expr.Type()); i++ {
							if _, err := fmt.Fprintf(w, " %%%d = extractvalue [%d x i8*] %%%d, %d %%%d = insertvalue [%d x i8*] %%%d, i8* %%%d, %d", ssaTemp, importedCount(st.Expr.Type()), imp, i, ssaTemp+1, importedCount(lvalue.Var.Type), ssaTemp-1, ssaTemp, limpOffset+i); err != nil {
								return err
							}
							ssaTemp += 2
						}
						if _, err := fmt.Fprintf(w, " %%import%d = select i1, [%d x i8*] %%%d, [%d x i8*] undef", ann.localsOnExit[lvalue.Var.Name], importedCount(lvalue.Var.Type), ssaTemp-1, importedCount(lvalue.Var.Type)); err != nil {
							return err
						}
					}
					if err := writeRef(ann.localsOnExit[lvalue.Var.Name], lvalue.Var.Type); err != nil {
						return err
					}
					if err := writeExprUnref(val, offs, imp, st.Expr.Type()); err != nil {
						return err
					}
					if err := writeUnref(ann.localsOnEntry[lvalue.Var.Name], lvalue.Var.Type); err != nil {
						return err
					}
					break loop
				case *ExprFunc:
					val, offs, imp, err := writeExpr(stmt, st.Expr, inLoop)
					if err != nil {
						return err
					}
					if _, err := fmt.Fprintf(w, " call void @__copy({%s, [0 x i1]}* %%%d, %s %%%d, {%s, [0 x i1]}* %%%d, %s %%%d, %s %d)", refCountType, val, offsetType, offs, refCountType, lval, offsetType, loffs, offsetType, st.Expr.Type().BitSize()); err != nil {
						return err
					}
					// don't need to copy %imports - not referenced anywhere
					if err := writeExprUnref(val, offs, imp, st.Expr.Type()); err != nil {
						return err
					}
					break loop
				default:
					panic("Unknown expr type")
				}
			}
			if err := writeExprUnref(lval, loffs, limp, st.Expr.Type()); err != nil {
				return err
			}
			if err := writeUnrefs(st.Next); err != nil {
				return err
			}
			if err := writeGotoNext(st.Next); err != nil {
				return err
			}
		case *StmtExpr:
			if val, offs, imp, err := writeExpr(stmt, st.Expr, inLoop); err != nil {
				return err
			} else if err := writeExprUnref(val, offs, imp, st.Expr.Type()); err != nil {
				return err
			}
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
