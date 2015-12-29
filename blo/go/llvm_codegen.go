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
			buf.WriteString(fmt.Sprintf("_%x_", rune))
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
	if _, err := io.WriteString(w, fmt.Sprintf("define void @__clear({%s, [0 x i1]}* %%v, %s %%bitsize) { br label %%l0 l0: %%1 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%v, i32 0, i32 0 store %s 0, %s* %%1 br label %%l1 l1: %%2 = phi %s [0, %%l0], [%%5, %%l2] %%3 = icmp ult %s %%2, %%bitsize br i1 %%3, label %%l2, label %%l3 l2: %%4 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%v, i32 0, i32 1, %s %%2 store i1 0, i1* %%4 %%5 = add %s %%2, 1 br label %%l1 l3: ret void }", refCountType, offsetType, refCountType, refCountType, offsetType, offsetType, offsetType, offsetType, refCountType, refCountType, offsetType, offsetType)); err != nil {
		return err
	}
	if _, err := io.WriteString(w, fmt.Sprintf("define void @__ref({%s, [0 x i1]}* %%v) { %%1 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%v, i32 0, i32 0 %%2 = load %s, %s* %%1 %%3 = add %s %%2, 1 store %s %%3, %s* %%1 ret void }", refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, refCountType)); err != nil {
		return err
	}
	if _, err := io.WriteString(w, fmt.Sprintf("define void @__unref({%s, [0 x i1]}* %%v) { %%1 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%v, i32 0, i32 0 %%2 = load %s, %s* %%1 %%3 = sub %s %%2, 1 store %s %%3, %s* %%1 ret void }", refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, refCountType)); err != nil {
		return err
	}
	if _, err := io.WriteString(w, fmt.Sprintf("define void @__copy({%s, [0 x i1]}* %%srcval, %s %%srcoffset, {%s, [0 x i1]}* %%destval, %s %%destoffset, %s %%bitsize) { br label %%l1 l1: %%1 = phi %s [0, %%0], [%%8, %%l2] %%2 = icmp ult %s %%1, %%bitsize br i1 %%2, label %%l2, label %%l3 l2: %%3 = add %s %%1, %%srcoffset %%4 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%srcval, i32 0, i32 1, %s %%3 %%5 = load i1, i1* %%4 %%6 = add %s %%1, %%destoffset %%7 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%destval, i32 0, i32 1, %s %%6 store i1 %%5, i1* %%7 %%8 = add %s %%1, 1 br label %%l1 l3: ret void }", refCountType, offsetType, refCountType, offsetType, offsetType, offsetType, offsetType, offsetType, refCountType, refCountType, offsetType, offsetType, refCountType, refCountType, offsetType, offsetType)); err != nil {
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
		if _, err := io.WriteString(w, fmt.Sprintf(" call void @__clear({%s, [0 x i1]}* %%%v, %s %%bitsize) ret {%s, [0 x i1]}* %%%d }", refCountType, v, offsetType, refCountType, v)); err != nil {
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
			if lvalue, ok := st.LValue.(*ExprVar); ok {
				ann.localsOnExit = make(map[string]int)
				for k, v := range ann.localsOnEntry {
					ann.localsOnExit[k] = v
				}
				ann.localsOnExit[lvalue.Name] = local
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
	ssaTemp := 0
	var typeNames []string
	for typeName, _ := range ast.Types {
		typeNames = append(typeNames, typeName)
	}
	sort.Strings(typeNames)
	for _, typeName := range typeNames {
		if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* null, i32 0, i32 1, %s %d %%sizeof.%s = ptrtoint i1* %%%d to %s", ssaTemp, refCountType, refCountType, offsetType, ast.Types[typeName].BitSize(), LLVMCanonicalName(typeName), ssaTemp, offsetType)); err != nil {
			return err
		}
		ssaTemp += 1
	}
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		ann := stmt.LLVMAnnotation()
		for _, alloca := range ann.allocas {
			if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = alloca i8, %s %%sizeof.%s %%alloca%d = bitcast i8* %%%d to {%s, [0 x i1]}*", ssaTemp, offsetType, LLVMCanonicalName(ann.allocaType.Name), alloca, ssaTemp, refCountType)); err != nil {
				return err
			}
			ssaTemp += 1
		}
		return nil
	})
	WalkExprs(funcDecl, func(stmt Stmt, inLoop bool, expr Expr, inAssign bool) error {
		ann := expr.LLVMAnnotation()
		for _, alloca := range ann.allocas {
			if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = alloca i8, %s %%sizeof.%s %%alloca%d = bitcast i8* %%%d to {%s, [0 x i1]}*", ssaTemp, offsetType, LLVMCanonicalName(ann.allocaType.Name), alloca, ssaTemp, refCountType)); err != nil {
				return err
			}
			ssaTemp += 1
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
	var writeExpr func(Stmt, Expr, bool) (int, int, error)
	writeExpr = func(stmt Stmt, expr Expr, inLoop bool) (int, int, error) {
		switch ex := expr.(type) {
		case *ExprVar:
			if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = select i1 1, {%s, [0 x i1]}* %%value%d, {%s, [0 x i1]}* null %%%d = select i1 1, %s %%offset%d, %s 0", ssaTemp, refCountType, stmt.LLVMAnnotation().localsOnEntry[ex.Name], refCountType, ssaTemp+1, offsetType, stmt.LLVMAnnotation().localsOnEntry[ex.Name], offsetType)); err != nil {
				return 0, 0, err
			}
			ssaTemp += 2
			return ssaTemp - 2, ssaTemp - 1, nil
		case *ExprField:
			val, offs, err := writeExpr(stmt, ex.Expr, inLoop)
			if err != nil {
				return 0, 0, err
			}
			if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = add %s %%%d, %d", ssaTemp, offsetType, offs, ex.Expr.Type().BitOffset(ex.Name))); err != nil {
				return 0, 0, err
			}
			ssaTemp += 1
			return val, ssaTemp - 1, nil
		case *ExprFunc:
			var args [][2]int
			for _, param := range ex.Params {
				val, offs, err := writeExpr(stmt, param, inLoop)
				if err != nil {
					return 0, 0, err
				}
				args = append(args, [2]int{val, offs})
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
					return 0, 0, err
				}
			case 1:
				retValAlloc = fmt.Sprintf("%%alloca%d", exprAnn.allocas[0])
				retVal = ssaTemp
				ssaTemp++
				if inLoop {
					if _, err := io.WriteString(w, fmt.Sprintf(" call void @__clear({%s, [0 x i1]}* %s, %s %d)", refCountType, retValAlloc, offsetType, exprAnn.allocaType.BitSize())); err != nil {
						return 0, 0, err
					}
				}
				if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = call {{%s, [0 x i1]}*, %s}", retVal, refCountType, offsetType)); err != nil {
					return 0, 0, err
				}
			default:
				retValAlloc = fmt.Sprintf("%%%d", ssaTemp)
				retVal = ssaTemp + 1
				ssaTemp += 2
				if _, err := io.WriteString(w, fmt.Sprintf(" %s = call {%s, [0 x i1]}* @__alloc%d(", retValAlloc, refCountType, len(exprAnn.allocas))); err != nil {
					return 0, 0, err
				}
				comma := ""
				for _, alloca := range exprAnn.allocas {
					if _, err := io.WriteString(w, fmt.Sprintf("%s{%s, [0 x i1]}* %%alloca%d", comma, refCountType, alloca)); err != nil {
						return 0, 0, err
					}
					comma = ","
				}
				if _, err := io.WriteString(w, fmt.Sprintf(") %%%d = call {{%s, [0 x i1]}*, %s}", retVal, refCountType, offsetType)); err != nil {
					return 0, 0, err
				}
			}
			if _, err := io.WriteString(w, fmt.Sprintf(" @%s(", LLVMCanonicalName(ex.Name))); err != nil {
				return 0, 0, err
			}
			comma := ""
			for _, arg := range args {
				if _, err := io.WriteString(w, fmt.Sprintf("%s{%s, [0 x i1]}* %%%d, %s %%%d", comma, refCountType, arg[0], offsetType, arg[1])); err != nil {
					return 0, 0, err
				}
				comma = ","
			}
			if len(exprAnn.allocas) > 0 {
				if _, err := io.WriteString(w, fmt.Sprintf("%s{%s, [0 x i1]}* %s", comma, refCountType, retValAlloc)); err != nil {
					return 0, 0, err
				}
			}
			if _, err := io.WriteString(w, ")"); err != nil {
				return 0, 0, err
			}
			if len(exprAnn.allocas) == 1 {
				return 0, 0, nil
			}
			val := ssaTemp
			offs := ssaTemp + 1
			ssaTemp += 2
			if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = extractvalue {{%s, [0 x i1]}*, %s} %%%d, i32 0 %%%d = extractvalue {{%s [0 x i1]}*, %s} %%%d, i32 1", val, refCountType, offsetType, retVal, offs, refCountType, offsetType, retVal)); err != nil {
				return 0, 0, err
			}
			return val, offs, nil
		default:
			panic("Unknown expr type")
		}
	}
	WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
		ann := stmt.LLVMAnnotation()
		if ann.startBlock {
			if _, err := io.WriteString(w, fmt.Sprintf(" block%d:", ann.blockLabel)); err != nil {
				return err
			}
			if len(ann.comesFrom) > 1 {
				var vars []string
				for name, _ := range ann.localsOnEntry {
					vars = append(vars, name)
				}
				sort.Strings(vars)
				for _, v := range vars {
					if _, err := io.WriteString(w, fmt.Sprintf(" %%value%d = phi {%s, [0 x i1]}* ", ann.localsOnEntry[v], refCountType)); err != nil {
						return err
					}
					comma := ""
					for _, prev := range ann.comesFrom {
						if _, err := io.WriteString(w, fmt.Sprintf("%s[%%value%d,%%block%d]", comma, prev.LLVMAnnotation().localsOnExit[v], prev.LLVMAnnotation().blockLabel)); err != nil {
							return err
						}
						comma = ","
					}
					if _, err := io.WriteString(w, fmt.Sprintf(" %%offset%d = phi %s ", ann.localsOnEntry[v], offsetType)); err != nil {
						return err
					}
					comma = ""
					for _, prev := range ann.comesFrom {
						if _, err := io.WriteString(w, fmt.Sprintf("%s[%%offset%d,%%block%d]", comma, prev.LLVMAnnotation().localsOnExit[v], prev.LLVMAnnotation().blockLabel)); err != nil {
							return err
						}
						comma = ","
					}
				}
			}
		}
		writeUnrefs := func(next Stmt) error {
			var unrefs []int
			for name, ref := range ann.localsOnExit {
				if next != nil {
					if _, ok := next.LLVMAnnotation().localsOnEntry[name]; ok {
						continue
					}
				}
				unrefs = append(unrefs, ref)
			}
			sort.Ints(unrefs)
			for _, unref := range unrefs {
				if _, err := io.WriteString(w, fmt.Sprintf(" call void @__unref({%s, [0 x i1]}* %%value%d)", refCountType, unref)); err != nil {
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
			if st.Expr != nil {
				val, offs, err := writeExpr(stmt, st.Expr, inLoop)
				if err != nil {
					return err
				}
				if _, err := io.WriteString(w, fmt.Sprintf(" %%value%d = select i1 1, {%s, [0 x i1]}* %%%d, {%s, [0 x i1]}* null %%offset%d = select i1 1, %s %%%d, %s 0", ann.localsOnExit[st.Var.Name], refCountType, val, refCountType, ann.localsOnExit[st.Var.Name], offsetType, offs, offsetType)); err != nil {
					return err
				}
			} else {
				switch len(ann.allocas) {
				case 0:
					panic("No allocations for var statement")
				case 1:
					if _, err := io.WriteString(w, fmt.Sprintf(" %%value%d = select i1 1, {%s, [0 x i1]}* %%alloca%d, {%s, [0 x i1]}* null %%offset%d = select i1 1, %s 0, %s 0", ann.localsOnExit[st.Var.Name], refCountType, ann.allocas[0], refCountType, ann.localsOnExit[st.Var.Name], offsetType, offsetType)); err != nil {
						return err
					}
					if inLoop {
						if _, err := io.WriteString(w, fmt.Sprintf(" call void @__clear({%s, [0 x i1]}* %%value%d, %s %d)", refCountType, ann.localsOnExit[st.Var.Name], offsetType, st.Var.Type.BitSize())); err != nil {
							return err
						}
					}
				default:
					if _, err := io.WriteString(w, fmt.Sprintf(" %%value%d = call {%s, [0 x i1]}* @__alloc%d(", ann.localsOnExit[st.Var.Name], refCountType, len(ann.allocas))); err != nil {
						return err
					}
					comma := ""
					for _, alloca := range ann.allocas {
						if _, err := io.WriteString(w, fmt.Sprintf("%s{%s, [0 x i1]}* %%alloca%d", comma, refCountType, alloca)); err != nil {
							return err
						}
						comma = ","
					}
					if _, err := io.WriteString(w, fmt.Sprintf(") %%offset%d = select i1 1, %s 0, %s 0", ann.localsOnExit[st.Var.Name], offsetType, offsetType)); err != nil {
						return err
					}
				}
			}
			if _, err := io.WriteString(w, fmt.Sprintf(" call void @__ref({%s, [0 x i1]}* %%value%d)", refCountType, ann.localsOnExit[st.Var.Name])); err != nil {
				return err
			}
			if err := writeUnrefs(st.Next); err != nil {
				return err
			}
			if err := writeGotoNext(st.Next); err != nil {
				return err
			}
		case *StmtIf:
			val, offs, err := writeExpr(stmt, st.Expr, inLoop)
			if err != nil {
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
			if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%%d, i32 0, i32 1, %s %%%d %%%d = load i1, i1* %%%d br i1 %%%d, label %%block%d, label %s", addr, refCountType, refCountType, val, offsetType, offs, cond, addr, cond, st.Stmts.LLVMAnnotation().blockLabel, elseBlock)); err != nil {
				return err
			}
			if writeElseReturn {
				if _, err := io.WriteString(w, fmt.Sprintf(" block%d.0:", ann.blockLabel)); err != nil {
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
			if st.Expr == nil {
				if err := writeUnrefs(nil); err != nil {
					return err
				}
				if _, err := io.WriteString(w, " ret void"); err != nil {
					return err
				}
			} else {
				val, offs, err := writeExpr(stmt, st.Expr, inLoop)
				if err != nil {
					return err
				}
				if err := writeUnrefs(nil); err != nil {
					return err
				}
				subblockLabel := 0
				for i, param := range funcDecl.Params {
					if param.Type != st.Expr.Type() && !param.Type.Contains(st.Expr.Type()) {
						continue
					}
					if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = icmp eq {%s, [0 x i1]}* %%%d, %%value%d br i1 %%%d, label %%block%d.%d, label %%block%d.%d block%d.%d: %%%d = insertvalue {{%s, [0 x i1]}*, %s} {{%s, [0 x i1]}* null, %s 0}, {%s, [0 x i1]}* %%%d, 0 %%%d = insertvalue {{%s, [0 x i1]}*, %s} %%%d, %s %%%d, 1 ret {{%s, [0 x i1]}*, %s} %%%d block%d.%d:", ssaTemp, refCountType, val, i, ssaTemp, ann.blockLabel, subblockLabel, ann.blockLabel, subblockLabel+1, ann.blockLabel, subblockLabel, ssaTemp+1, refCountType, offsetType, refCountType, offsetType, refCountType, val, ssaTemp+2, refCountType, offsetType, ssaTemp+1, offsetType, offs, refCountType, offsetType, ssaTemp+2, ann.blockLabel, subblockLabel+1)); err != nil {
						return err
					}
					subblockLabel += 2
					ssaTemp += 3
				}
				if _, err := io.WriteString(w, fmt.Sprintf(" call void @__copy({%s, [0 x i1]}* %%%d, %s %%%d, {%s, [0 x i1]}* %%retval, %s 0, %s %d) %%%d = insertvalue {{%s, [0 x i1]}*, %s} {{%s, [0 x i1]}* null, %s 0}, {%s, [0 x i1]}* %%retval, 0 ret {{%s, [0 x i1]}*, %s} %%%d", refCountType, val, offsetType, offs, refCountType, offsetType, offsetType, st.Expr.Type().BitSize(), ssaTemp, refCountType, offsetType, refCountType, offsetType, refCountType, refCountType, offsetType, ssaTemp)); err != nil {
					return err
				}
				ssaTemp++
			}
		case *StmtSetClear:
			val, offs, err := writeExpr(stmt, st.Expr, inLoop)
			if err != nil {
				return err
			}
			addr := ssaTemp
			ssaTemp++
			bit := 0
			if st.Value {
				bit = 1
			}
			if _, err := io.WriteString(w, fmt.Sprintf(" %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%%d, i32 0, i32 1, %s %%%d store i1 %d, i1* %%%d", addr, refCountType, refCountType, val, offsetType, offs, bit, addr)); err != nil {
				return err
			}
			if err := writeUnrefs(st.Next); err != nil {
				return err
			}
			if err := writeGotoNext(st.Next); err != nil {
				return err
			}
		case *StmtAssign:
			if lvalue, ok := st.LValue.(*ExprVar); ok {
				val, offs, err := writeExpr(stmt, st.Expr, inLoop)
				if err != nil {
					return err
				}
				if _, err := io.WriteString(w, fmt.Sprintf(" call void @__unref({%s, [0 x i1]}* %%value%d) %%value%d = select i1 1, {%s, [0 x i1]}* %%%d, {%s, [0 x i1]}* null %%offset%d = select i1 1, %s %%%d, %s 0 call void @__ref({%s, [0 x i1]}* %%value%d)", refCountType, ann.localsOnEntry[lvalue.Var.Name], ann.localsOnExit[lvalue.Var.Name], refCountType, val, refCountType, ann.localsOnExit[lvalue.Var.Name], offsetType, offs, offsetType, refCountType, ann.localsOnExit[lvalue.Var.Name])); err != nil {
					return err
				}
			} else {
				lval, loffs, lerr := writeExpr(stmt, st.LValue, inLoop)
				if lerr != nil {
					return lerr
				}
				val, offs, err := writeExpr(stmt, st.Expr, inLoop)
				if err != nil {
					return err
				}
				if _, err := io.WriteString(w, fmt.Sprintf(" call void @__copy({%s, [0 x i1]}* %%%d, %s %%%d, {%s, [0 x i1]}* %%%d, %s %%%d, %s %d)", refCountType, val, offsetType, offs, refCountType, lval, offsetType, loffs, offsetType, st.Expr.Type().BitSize())); err != nil {
					return err
				}
			}
			if err := writeUnrefs(st.Next); err != nil {
				return err
			}
			if err := writeGotoNext(st.Next); err != nil {
				return err
			}
		case *StmtExpr:
			if _, _, err := writeExpr(stmt, st.Expr, inLoop); err != nil {
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
