package main

// Second implementation
// Converts AST to stack-based VM to enable tail-call elimination

import (
	"fmt"
	"io"
)

func Interp2(r io.Reader, w io.Writer, stmts []Stmt) error {
	var enclosing *analyzeScope2 = nil
	main := &fn2{}
	if err := compile2(main, enclosing, []Token{}, &StmtBlock{
		Token:  Token{},
		Stmts:  stmts,
		Expr:   nil,
		Return: true,
	}); err != nil {
		return err
	}
	return interp2(r, w, &frame2{
		caller:    nil,
		enclosing: nil,
		idents:    make([]ident2, main.nIdents),
		stack:     []ident2{},
		insns:     main.insns,
		insnIndex: 0,
	})
}

type ident2 struct {
	version int
	value   *Value
}

func newIdent2(val *Value) ident2 {
	return ident2{version: val.Version(), value: val}
}

func (id ident2) valid() bool {
	return id.version == id.value.Version()
}

type frame2 struct {
	caller    *frame2
	enclosing *frame2
	idents    []ident2
	stack     []ident2

	insns     []insn2
	insnIndex int
}

type fn2 struct {
	insns   []insn2
	nArgs   int
	nIdents int
}

type insn2 interface {
	insnSrcToken() Token
}

// insn2s:                                   stack:
// - val0                                    -  - : val
// - valIdent [enclosingLevel] [identIndex]  -  - : val
// - call [tailCall] [enclosingLevel] [*fn2] -  args... : result
// - callLibFunc [LibFunc]                   -  args... : result
// - pushExpr                                -  left right : result
// - tryPushExpr [insnIndex]                 -  left right : result on branch or - on no branch
// - popExpr [identIndex] [insnIndex]        -  arg : result on branch or - on no branch
// - gtExpr [insnIndex]                      -  left right : result on branch or - on no branch
// - ltExpr [insnIndex]                      -  left right : result on branch or - on no branch
// - eqExpr [insnIndex]                      -  left right : result on branch or - on no branch
// - pop                                     -  val : -
// - ret                                     -  result : result (caller stack)

type insn2Token Token

func (insn *insn2Token) insnSrcToken() Token {
	return Token(*insn)
}

func (insn *insn2Token) errorf(format string, a ...interface{}) error {
	return Token(*insn).Errorf(format, a...)
}

type insn2Val0 struct {
	insn2Token
}

type insn2ValIdent struct {
	insn2Token
	enclosingLevel int
	identIndex     int
}

type insn2Assign struct {
	insn2Token
	identIndex int
}

type insn2Call struct {
	insn2Token
	tailCall       bool
	enclosingLevel int
	fn             *fn2
}

type insn2CallLibFunc struct {
	insn2Token
	fn LibFunc
}

type insn2PushExpr struct {
	insn2Token
}

type insn2TryPushExpr struct {
	insn2Token
	insnIndex int
}

type insn2PopExpr struct {
	insn2Token
	identIndex int
	insnIndex  int
}

type insn2GtExpr struct {
	insn2Token
	insnIndex int
}

type insn2LtExpr struct {
	insn2Token
	insnIndex int
}

type insn2EqExpr struct {
	insn2Token
	insnIndex int
}

type insn2Pop struct {
	insn2Token
}

type insn2Ret struct {
	insn2Token
}

func interp2(r io.Reader, w io.Writer, frame *frame2) error {
	for {
		if frame.insnIndex >= len(frame.insns) {
			return nil
		}
		switch insn := frame.insns[frame.insnIndex].(type) {
		case *insn2Val0:
			frame.stack = append(frame.stack, newIdent2(&Value{}))
			frame.insnIndex++
		case *insn2ValIdent:
			enclosing := frame
			for i := 0; i < insn.enclosingLevel; i++ {
				enclosing = enclosing.enclosing
			}
			ident := enclosing.idents[insn.identIndex]
			if !ident.valid() {
				return insn.errorf("Identifier refers to an inaccessible value")
			}
			frame.stack = append(frame.stack, ident)
			frame.insnIndex++
		case *insn2Assign:
			ident := frame.stack[len(frame.stack)-1]
			if !ident.valid() {
				return insn.errorf("Assigning an inaccessible value")
			}
			frame.stack = frame.stack[:len(frame.stack)-1]
			frame.idents[insn.identIndex] = ident
			frame.insnIndex++
		case *insn2Call:
			enclosing := frame
			for i := 0; i < insn.enclosingLevel; i++ {
				enclosing = enclosing.enclosing
			}
			caller := frame
			if insn.tailCall {
				caller = frame.caller
			}
			newFrame := &frame2{
				caller:    caller,
				enclosing: enclosing,
				idents:    make([]ident2, insn.fn.nIdents),
				stack:     []ident2{},
				insns:     insn.fn.insns,
				insnIndex: 0,
			}
			for i := 0; i < insn.fn.nArgs; i++ {
				newFrame.idents[i] = frame.stack[len(frame.stack)-insn.fn.nArgs+i]
			}
			frame.stack = frame.stack[:len(frame.stack)-insn.fn.nArgs]
			frame = newFrame
		case *insn2CallLibFunc:
			args := make([]*Value, insn.fn.ArgCount())
			for i := 0; i < insn.fn.ArgCount(); i++ {
				arg := frame.stack[len(frame.stack)-insn.fn.ArgCount()+i]
				if !arg.valid() {
					return insn.errorf("Argument %d refers to an inaccessible value", i+1)
				}
				args[i] = arg.value
			}
			result, _, err := insn.fn.Call(insn.insnSrcToken(), r, w, args)
			if err != nil {
				return err
			}
			frame.stack = append(frame.stack[:len(frame.stack)-insn.fn.ArgCount()], newIdent2(result))
			frame.insnIndex++
		case *insn2PushExpr:
			left := frame.stack[len(frame.stack)-2]
			if !left.valid() {
				return insn.errorf("Left argument refers to an inaccessible value")
			}
			right := frame.stack[len(frame.stack)-1]
			if !right.valid() {
				return insn.errorf("Right argument refers to an inaccessible value")
			}
			if left.value == right.value {
				return insn.errorf("Expression pushes a value onto itself")
			}
			left.value.Push(right.value)
			frame.stack = frame.stack[:len(frame.stack)-1]
			frame.insnIndex++
		case *insn2TryPushExpr:
			left := frame.stack[len(frame.stack)-2]
			if !left.valid() {
				return insn.errorf("Left argument refers to an inaccessible value")
			}
			right := frame.stack[len(frame.stack)-1]
			if !right.valid() {
				return insn.errorf("Right argument refers to an inaccessible value")
			}
			if left.value == right.value {
				return insn.errorf("Expression pushes a value onto itself")
			}
			if left.value.TryPush(right.value) {
				frame.stack = frame.stack[:len(frame.stack)-1]
				frame.insnIndex = insn.insnIndex
			} else {
				frame.stack = frame.stack[:len(frame.stack)-2]
				frame.insnIndex++
			}
		case *insn2PopExpr:
			val := frame.stack[len(frame.stack)-1]
			if !val.valid() {
				return insn.errorf("Argument refers to an inaccessible value")
			}
			popped := val.value.Pop()
			if popped == nil {
				frame.insnIndex = insn.insnIndex
			} else {
				frame.idents[insn.identIndex] = newIdent2(popped)
				frame.stack = frame.stack[:len(frame.stack)-1]
				frame.insnIndex++
			}
		case *insn2GtExpr:
			left := frame.stack[len(frame.stack)-2]
			if !left.valid() {
				return insn.errorf("Left argument refers to an inaccessible value")
			}
			right := frame.stack[len(frame.stack)-1]
			if !right.valid() {
				return insn.errorf("Right argument refers to an inaccessible value")
			}
			if left.value.Size() > right.value.Size() {
				frame.stack = frame.stack[:len(frame.stack)-1]
				frame.insnIndex++
			} else {
				frame.stack[len(frame.stack)-2] = right
				frame.stack = frame.stack[:len(frame.stack)-1]
				frame.insnIndex = insn.insnIndex
			}
		case *insn2LtExpr:
			left := frame.stack[len(frame.stack)-2]
			if !left.valid() {
				return insn.errorf("Left argument refers to an inaccessible value")
			}
			right := frame.stack[len(frame.stack)-1]
			if !right.valid() {
				return insn.errorf("Right argument refers to an inaccessible value")
			}
			if left.value.Size() < right.value.Size() {
				frame.stack = frame.stack[:len(frame.stack)-1]
				frame.insnIndex++
			} else {
				frame.stack[len(frame.stack)-2] = right
				frame.stack = frame.stack[:len(frame.stack)-1]
				frame.insnIndex = insn.insnIndex
			}
		case *insn2EqExpr:
			left := frame.stack[len(frame.stack)-2]
			if !left.valid() {
				return insn.errorf("Left argument refers to an inaccessible value")
			}
			right := frame.stack[len(frame.stack)-1]
			if !right.valid() {
				return insn.errorf("Right argument refers to an inaccessible value")
			}
			if left.value.Size() == right.value.Size() {
				frame.stack = frame.stack[:len(frame.stack)-1]
				frame.insnIndex++
			} else {
				frame.stack[len(frame.stack)-2] = right
				frame.stack = frame.stack[:len(frame.stack)-1]
				frame.insnIndex = insn.insnIndex
			}
		case *insn2Pop:
			frame.stack = frame.stack[:len(frame.stack)-1]
			frame.insnIndex++
		case *insn2Ret:
			if frame.caller == nil {
				return nil
			}
			result := frame.stack[len(frame.stack)-1]
			frame = frame.caller
			frame.stack = append(frame.stack, result)
			frame.insnIndex++
		default:
			panic("Unknown insn2")
		}
	}
}

type analyzeIdent2 struct {
	name  Token
	index int
}

type analyzeFn2 struct {
	name    Token
	fn      *fn2
	libFunc LibFunc
}

type analyzeScope2 struct {
	parent         *analyzeScope2
	enclosing      *analyzeScope2
	idents         map[string]*analyzeIdent2
	fns            map[string]*analyzeFn2
	nextIdentIndex int
}

func compile2(fn *fn2, enclosing *analyzeScope2, args []Token, block *StmtBlock) error {
	fn.nArgs = len(args)
	scope := &analyzeScope2{
		parent:         nil,
		enclosing:      enclosing,
		idents:         make(map[string]*analyzeIdent2),
		fns:            make(map[string]*analyzeFn2),
		nextIdentIndex: 0,
	}
	for _, arg := range args {
		scope.idents[arg.Value] = &analyzeIdent2{
			name:  arg,
			index: scope.nextIdentIndex,
		}
		scope.nextIdentIndex++
	}
	if err := compileBlock2(fn, scope, block, true); err != nil {
		return err
	}
	fn.nIdents = scope.nextIdentIndex
	return nil
}

func compileBlock2(fn *fn2, scope *analyzeScope2, block *StmtBlock, isFnBody bool) error {
	for _, statement := range block.Stmts {
		switch stmt := statement.(type) {
		case *StmtDefineFunc:
			if shadowedFn, ok := scope.fns[stmt.Name.Value]; ok {
				return stmt.Name.Errorf("Function definition of %s shadows definition at %s:%d:%d", stmt.Name.Value, shadowedFn.name.Filename, shadowedFn.name.Line, shadowedFn.name.Column)
			}
			scope.fns[stmt.Name.Value] = &analyzeFn2{name: stmt.Name, fn: &fn2{nArgs: len(stmt.Params)}}
		case *StmtDefineLibFunc:
			if shadowedFn, ok := scope.fns[stmt.Name.Value]; ok {
				return stmt.Name.Errorf("Function definition of %s shadows definition at %s:%d:%d", stmt.Name.Value, shadowedFn.name.Filename, shadowedFn.name.Line, shadowedFn.name.Column)
			}
			libFunc, err := GetLibFunc(stmt.Lib, len(stmt.Params))
			if err != nil {
				return err
			}
			scope.fns[stmt.Name.Value] = &analyzeFn2{name: stmt.Name, libFunc: libFunc}
		}
	}
	for _, stmt := range block.Stmts {
		if err := compileStmt2(fn, scope, stmt); err != nil {
			return err
		}
	}
	if block.Expr != nil {
		if err := compileExpr2(fn, scope, block.Expr, block.Return || isFnBody); err != nil {
			return err
		}
	}
	return nil
}

func compileStmt2(fn *fn2, scope *analyzeScope2, statement Stmt) error {
	switch stmt := statement.(type) {
	case *StmtAssignment:
		if err := compileExpr2(fn, scope, stmt.Expr, false); err != nil {
			return err
		}
		identIndex := scope.nextIdentIndex
		scope.nextIdentIndex++
		scope.idents[stmt.Name.Value] = &analyzeIdent2{
			name:  stmt.Name,
			index: identIndex,
		}
		fn.insns = append(fn.insns, &insn2Assign{
			insn2Token: insn2Token(stmt.StmtFirstToken()),
			identIndex: identIndex,
		})
	case *StmtDefineFunc:
		if err := compile2(scope.fns[stmt.Name.Value].fn, scope, stmt.Params, &stmt.Body); err != nil {
			return err
		}
	case *StmtDefineLibFunc:
		libFunc, err := GetLibFunc(stmt.Lib, len(stmt.Params))
		if err != nil {
			return err
		}
		scope.fns[stmt.Name.Value] = &analyzeFn2{libFunc: libFunc}
	case *StmtExpr:
		if err := compileExpr2(fn, scope, stmt.Expr, false); err != nil {
			return err
		}
		fn.insns = append(fn.insns, &insn2Pop{insn2Token(stmt.StmtFirstToken())})
	case *StmtBlock:
		blockScope := &analyzeScope2{
			parent:         scope,
			enclosing:      nil,
			idents:         make(map[string]*analyzeIdent2),
			fns:            make(map[string]*analyzeFn2),
			nextIdentIndex: scope.nextIdentIndex,
		}
		if err := compileBlock2(fn, blockScope, stmt, false); err != nil {
			return nil
		}
		if !stmt.Return {
			fn.insns = append(fn.insns, &insn2Pop{insn2Token(stmt.Expr.ExprFirstToken())})
		}
		scope.nextIdentIndex = blockScope.nextIdentIndex
	default:
		panic("Unknown statement")
	}
	return nil
}

func compileExpr2(fn *fn2, scope *analyzeScope2, expression Expr, isReturn bool) error {
	switch expr := expression.(type) {
	case *Expr0:
		fn.insns = append(fn.insns, &insn2Val0{insn2Token(expr.ExprFirstToken())})
		if isReturn {
			fn.insns = append(fn.insns, &insn2Ret{insn2Token(expr.ExprFirstToken())})
		}
	case *ExprIdentifier:
		var ident *analyzeIdent2
		enclosingLevel := 0
		for s := scope; s != nil; {
			ident = s.idents[expr.Name.Value]
			if ident != nil {
				break
			}
			if s.parent != nil {
				s = s.parent
			} else {
				enclosingLevel++
				s = s.enclosing
			}
		}
		if ident == nil {
			return expr.Name.Errorf("Undefined identifier %s", expr.Name.Value)
		}
		fn.insns = append(fn.insns, &insn2ValIdent{
			insn2Token:     insn2Token(expr.Name),
			enclosingLevel: enclosingLevel,
			identIndex:     ident.index,
		})
		if isReturn {
			fn.insns = append(fn.insns, &insn2Ret{insn2Token(expr.ExprFirstToken())})
		}
	case *ExprCallFunction:
		var calledFn *analyzeFn2
		enclosingLevel := 0
		for s := scope; s != nil; {
			calledFn = s.fns[expr.Name.Value]
			if calledFn != nil {
				break
			}
			if s.parent != nil {
				s = s.parent
			} else {
				enclosingLevel++
				s = s.enclosing
			}
		}
		if calledFn == nil {
			return expr.Name.Errorf("Undefined function %s", expr.Name.Value)
		}
		for _, arg := range expr.Args {
			if err := compileExpr2(fn, scope, arg, false); err != nil {
				return err
			}
		}
		if calledFn.libFunc != nil {
			if len(expr.Args) != calledFn.libFunc.ArgCount() {
				return expr.Name.Errorf("Function %s called with %d argument(s), expected %d argument(s)", expr.Name.Value, len(expr.Args), calledFn.libFunc.ArgCount())
			}
			fn.insns = append(fn.insns, &insn2CallLibFunc{insn2Token(expr.Name), calledFn.libFunc})
			if isReturn {
				fn.insns = append(fn.insns, &insn2Ret{insn2Token(expr.Name)})
			}
		} else {
			if len(expr.Args) != calledFn.fn.nArgs {
				return expr.Name.Errorf("Function %s called with %d argument(s), expected %d argument(s)", expr.Name.Value, len(expr.Args), calledFn.fn.nArgs)
			}
			fn.insns = append(fn.insns, &insn2Call{insn2Token(expr.Name), isReturn, enclosingLevel, calledFn.fn})
		}
	case *ExprBinary:
		if err := compileExpr2(fn, scope, expr.Left, false); err != nil {
			return err
		}
		if err := compileExpr2(fn, scope, expr.Right, false); err != nil {
			return err
		}
		var insnIndex *int
		blockNeedsPop := true
		switch expr.Op.Value {
		case "+":
			if expr.Block == nil {
				fn.insns = append(fn.insns, &insn2PushExpr{insn2Token(expr.Op)})
			} else {
				insn := &insn2TryPushExpr{insn2Token(expr.Op), -1}
				fn.insns = append(fn.insns, insn)
				insnIndex = &insn.insnIndex
				blockNeedsPop = false
			}
		case "<":
			insn := &insn2LtExpr{insn2Token(expr.Op), -1}
			fn.insns = append(fn.insns, insn)
			insnIndex = &insn.insnIndex
		case ">":
			insn := &insn2GtExpr{insn2Token(expr.Op), -1}
			fn.insns = append(fn.insns, insn)
			insnIndex = &insn.insnIndex
		case "=":
			insn := &insn2EqExpr{insn2Token(expr.Op), -1}
			fn.insns = append(fn.insns, insn)
			insnIndex = &insn.insnIndex
		default:
			panic("Unknown binary operator")
		}
		if insnIndex != nil {
			if expr.Block != nil {
				if blockNeedsPop {
					fn.insns = append(fn.insns, &insn2Pop{insn2Token(expr.Block.Token)})
				}
				blockScope := &analyzeScope2{
					parent:         scope,
					enclosing:      nil,
					idents:         make(map[string]*analyzeIdent2),
					fns:            make(map[string]*analyzeFn2),
					nextIdentIndex: scope.nextIdentIndex,
				}
				if err := compileBlock2(fn, blockScope, expr.Block, false); err != nil {
					return err
				}
				scope.nextIdentIndex = blockScope.nextIdentIndex
			}
			*insnIndex = len(fn.insns)
		}
		if isReturn {
			fn.insns = append(fn.insns, &insn2Ret{insn2Token(expr.ExprFirstToken())})
		}
	case *ExprPop:
		if err := compileExpr2(fn, scope, expr.Expr, false); err != nil {
			return err
		}
		identIndex := scope.nextIdentIndex
		scope.nextIdentIndex++
		insn := &insn2PopExpr{
			insn2Token: insn2Token(expr.ExprFirstToken()),
			identIndex: identIndex,
			insnIndex:  -1,
		}
		fn.insns = append(fn.insns, insn)
		if expr.Block != nil {
			blockScope := &analyzeScope2{
				parent:         scope,
				enclosing:      nil,
				idents:         make(map[string]*analyzeIdent2),
				fns:            make(map[string]*analyzeFn2),
				nextIdentIndex: scope.nextIdentIndex,
			}
			blockScope.idents[expr.Block.Name.Value] = &analyzeIdent2{
				name:  expr.Block.Name,
				index: identIndex,
			}
			for _, blockStmt := range expr.Block.Block.Stmts {
				if err := compileStmt2(fn, blockScope, blockStmt); err != nil {
					return err
				}
			}
			if err := compileExpr2(fn, blockScope, expr.Block.Block.Expr, expr.Block.Block.Return); err != nil {
				return err
			}
			scope.nextIdentIndex = blockScope.nextIdentIndex
		}
		insn.insnIndex = len(fn.insns)
		if isReturn {
			fn.insns = append(fn.insns, &insn2Ret{insn2Token(expr.ExprFirstToken())})
		}
	default:
		panic("Unknown expression")
	}
	return nil
}

func Compile2(stmts []Stmt) ([]string, error) {
	var enclosing *analyzeScope2 = nil
	main := &fn2{}
	if err := compile2(main, enclosing, []Token{}, &StmtBlock{
		Token:  Token{},
		Stmts:  stmts,
		Expr:   nil,
		Return: true,
	}); err != nil {
		return nil, err
	}
	fns := []*fn2{main}
	findFnIndex := func(fn *fn2) (int, bool) {
		for i := range fns {
			if fn == fns[i] {
				return i, true
			}
		}
		fns = append(fns, fn)
		return len(fns) - 1, false
	}
	var collectFns func(fn *fn2)
	collectFns = func(fn *fn2) {
		for _, instruction := range fn.insns {
			if insn, ok := instruction.(*insn2Call); ok {
				if _, known := findFnIndex(insn.fn); !known {
					collectFns(insn.fn)
				}
			}
		}
	}
	collectFns(main)
	asm := []string{}
	for i, fn := range fns {
		asm = append(asm, fmt.Sprintf("fn%d nArgs:%d nIdents:%d", i, fn.nArgs, fn.nIdents))
		for insnIndex, instruction := range fn.insns {
			switch insn := instruction.(type) {
			case *insn2Val0:
				asm = append(asm, fmt.Sprintf("<%d>: val0", insnIndex))
			case *insn2ValIdent:
				asm = append(asm, fmt.Sprintf("<%d>: valIdent [%d]%d", insnIndex, insn.enclosingLevel, insn.identIndex))
			case *insn2Assign:
				asm = append(asm, fmt.Sprintf("<%d>: assign %d", insnIndex, insn.identIndex))
			case *insn2Call:
				tail := ""
				if insn.tailCall {
					tail = "tail"
				}
				fnIndex, _ := findFnIndex(insn.fn)
				asm = append(asm, fmt.Sprintf("<%d>: %scall[%d] fn%d nArgs:%d", insnIndex, tail, insn.enclosingLevel, fnIndex, insn.fn.nArgs))
			case *insn2CallLibFunc:
				asm = append(asm, fmt.Sprintf("<%d>: call lib %s nArgs:%d", insnIndex, Token(insn.insn2Token).Value, insn.fn.ArgCount()))
			case *insn2PushExpr:
				asm = append(asm, fmt.Sprintf("<%d>: pushexpr", insnIndex))
			case *insn2TryPushExpr:
				asm = append(asm, fmt.Sprintf("<%d>: trypushexpr <%d>", insnIndex, insn.insnIndex))
			case *insn2PopExpr:
				asm = append(asm, fmt.Sprintf("<%d>: popexpr %d <%d>", insnIndex, insn.identIndex, insn.insnIndex))
			case *insn2GtExpr:
				asm = append(asm, fmt.Sprintf("<%d>: gtexpr <%d>", insnIndex, insn.insnIndex))
			case *insn2LtExpr:
				asm = append(asm, fmt.Sprintf("<%d>: ltexpr <%d>", insnIndex, insn.insnIndex))
			case *insn2EqExpr:
				asm = append(asm, fmt.Sprintf("<%d>: eqexpr <%d>", insnIndex, insn.insnIndex))
			case *insn2Pop:
				asm = append(asm, fmt.Sprintf("<%d>: pop", insnIndex))
			case *insn2Ret:
				asm = append(asm, fmt.Sprintf("<%d>: ret", insnIndex))
			default:
				panic("Unknown insn2")
			}
		}
	}
	return asm, nil
}
