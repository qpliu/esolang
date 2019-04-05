package main

// Second implementation
// Converts AST to stack-based VM to enable tail-call elimination

import (
	"io"
)

func Interp2(r io.Reader, w io.Writer, stmts []Stmt) error {
	var enclosing *analyzeScope2 = nil
	var returnExpr Expr = nil
	f, err := compile2(enclosing, []Token{}, stmts, returnExpr)
	if err != nil {
		return err
	}
	return interp2(r, w, &frame2{
		caller:    nil,
		enclosing: nil,
		idents:    make([]ident2, f.nIdents),
		stack:     []ident2{},
		insns:     f.insns,
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

// insn2s:                                  stack:
// - jmp [insnIndex]                        -  - : -
// - val0                                   -  - : val
// - valIdent [enclosingLevel] [identIndex] -  - : val
// - call [enclosingLevel] [*fn2]           -  args... : result
// - tailCall [enclosingLevel] [*fn2]       -  args... : result (caller stack)
// - callLibFunc [LibFunc]                  -  args... : result
// - pushExpr                               -  left right : result
// - tryPushExpr [insnIndex]                -  left right : result on branch or - on no branch
// - popExpr [identIndex] [insnIndex]       -  arg : result on branch or - on no branch
// - gtExpr [insnIndex]                     -  left right : result on branch or - on no branch
// - ltExpr [insnIndex]                     -  left right : result on branch or - on no branch
// - eqExpr [insnIndex]                     -  left right : result on branch or - on no branch
// - pop                                    -  val : -
// - ret                                    -  result : result (caller stack)

type insn2Token Token

func (insn *insn2Token) insnSrcToken() Token {
	return Token(*insn)
}

func (insn *insn2Token) errorf(format string, a ...interface{}) error {
	return Token(*insn).Errorf(format, a...)
}

type insn2Jump struct {
	insn2Token
	insnIndex int
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
	enclosingLevel int
	fn             *fn2
}

type insn2TailCall struct {
	insn2Token
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
		case *insn2Jump:
			frame.insnIndex = insn.insnIndex
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
			newFrame := &frame2{
				caller:    frame,
				enclosing: enclosing,
				idents:    make([]ident2, insn.fn.nIdents),
				insnIndex: 0,
			}
			for i := 0; i < insn.fn.nArgs; i++ {
				newFrame.idents[i] = frame.stack[len(frame.stack)-insn.fn.nArgs+i]
			}
			frame.stack = frame.stack[:len(frame.stack)-insn.fn.nArgs]
			frame = newFrame
		case *insn2TailCall:
			enclosing := frame
			for i := 0; i < insn.enclosingLevel; i++ {
				enclosing = enclosing.enclosing
			}
			newFrame := &frame2{
				caller:    frame.caller,
				enclosing: enclosing,
				idents:    make([]ident2, insn.fn.nIdents),
				insnIndex: 0,
			}
			for i := 0; i < insn.fn.nArgs; i++ {
				newFrame.idents[i] = frame.stack[len(frame.stack)-insn.fn.nArgs+i]
			}
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

func compile2(enclosing *analyzeScope2, args []Token, stmts []Stmt, resultExpr Expr) (*fn2, error) {
	fn := &fn2{nArgs: len(args)}
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
	for _, stmt := range stmts {
		if err := compileStmt2(fn, scope, stmt); err != nil {
			return nil, err
		}
	}
	if resultExpr != nil {
		if err := compileExpr2(fn, scope, resultExpr, true); err != nil {
			return nil, err
		}
	}
	fn.nIdents = scope.nextIdentIndex
	return fn, nil
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
		definedFn, err := compile2(scope, stmt.Params, stmt.Body.Stmts, stmt.Body.Expr)
		if err != nil {
			return err
		}
		scope.fns[stmt.Name.Value] = &analyzeFn2{fn: definedFn}
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
		for _, blockStmt := range stmt.Stmts {
			if err := compileStmt2(fn, blockScope, blockStmt); err != nil {
				return err
			}
		}
		if err := compileExpr2(fn, blockScope, stmt.Expr, stmt.Return); err != nil {
			return err
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
		panic("Not implemented")
	case *ExprBinary:
		panic("Not implemented")
	case *ExprPop:
		panic("Not implemented")
	default:
		panic("Unknown expression")
	}
	return nil
}
