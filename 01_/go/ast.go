package main

import (
	"bytes"
	"fmt"
)

type Parameter struct {
	MatchToken Token
	Match      Bitlist
	Bound      bool
	Bind       Token
	Wild       bool
}

type Def struct {
	Name       Token
	Parameters []Parameter
	Body       Expr
}

type Expr interface {
	InitFuncalls(defs map[string][]*Def)
	Eval(bindings []Bitlist) Bitlist
}

type LiteralExpr struct {
	bits *LiteralBitlist
}

func NewLiteralExpr(bits *LiteralBitlist) *LiteralExpr {
	result := new(LiteralExpr)
	result.bits = bits
	return result
}

func (expr *LiteralExpr) InitFuncalls(defs map[string][]*Def) {
}

func (expr *LiteralExpr) Eval(bindings []Bitlist) Bitlist {
	return expr.bits
}

type ConcatExpr struct {
	first, last Expr
}

func NewConcatExpr(first, last Expr) *ConcatExpr {
	result := new(ConcatExpr)
	result.first, result.last = first, last
	return result
}

func (expr *ConcatExpr) InitFuncalls(defs map[string][]*Def) {
	expr.first.InitFuncalls(defs)
	expr.last.InitFuncalls(defs)
}

func (expr *ConcatExpr) Eval(bindings []Bitlist) Bitlist {
	return &concatBitlist{bindings, expr, nil, nil, nil}
}

type concatBitlist struct {
	bindings    []Bitlist
	expr        *ConcatExpr
	first, last Bitlist
	next        *concatBitlist
}

func (concat *concatBitlist) force() {
	if concat.first == nil {
		concat.first = concat.expr.first.Eval(concat.bindings)
	}
	if !concat.first.Nil() {
		return
	}
	if concat.last == nil {
		concat.last = concat.expr.last.Eval(concat.bindings)
	}
}

func (concat *concatBitlist) Nil() bool {
	concat.force()
	return concat.first.Nil() && concat.last.Nil()
}

func (concat *concatBitlist) Bit() bool {
	concat.force()
	if concat.first.Nil() {
		return concat.last.Bit()
	}
	return concat.first.Bit()
}

func (concat *concatBitlist) Next() Bitlist {
	concat.force()
	if concat.first.Nil() {
		return concat.last.Next()
	}
	if concat.next == nil {
		concat.next = &concatBitlist{concat.bindings, concat.expr, concat.first.Next(), concat.last, nil}
	}
	return concat.next
}

type BoundExpr struct {
	name  Token
	index int
}

func NewBoundExpr(name Token, index int) *BoundExpr {
	result := new(BoundExpr)
	result.name = name
	result.index = index
	return result
}

func (expr *BoundExpr) InitFuncalls(defs map[string][]*Def) {
}

func (expr *BoundExpr) Eval(bindings []Bitlist) Bitlist {
	return bindings[expr.index]
}

type FuncallExpr struct {
	name     Token
	defs     []*Def
	argExprs []Expr
}

func NewFuncallExpr(name Token, argExprs []Expr) *FuncallExpr {
	result := new(FuncallExpr)
	result.name = name
	result.argExprs = argExprs
	return result
}

func (expr *FuncallExpr) InitFuncalls(defs map[string][]*Def) {
	expr.defs = defs[expr.name.Token]
	for _, argExpr := range expr.argExprs {
		argExpr.InitFuncalls(defs)
	}
}

func (expr *FuncallExpr) Eval(bindings []Bitlist) Bitlist {
	return &funcallBitlist{bindings, expr, nil}
}

type funcallBitlist struct {
	bindings []Bitlist
	expr     *FuncallExpr
	result   Bitlist
}

func (funcall *funcallBitlist) force() {
	if funcall.result != nil {
		return
	}
	args := make([]Bitlist, 0, len(funcall.expr.argExprs))
	for _, argExpr := range funcall.expr.argExprs {
		args = append(args, argExpr.Eval(funcall.bindings))
	}
	funcall.result = EvalFn(funcall.expr.defs, args)
	if funcall.result == nil {
		panic(fmt.Sprintf("%s no matching def for %s", funcall.expr.name.Location(), funcall.expr.name.Token))
	}
}

func (funcall *funcallBitlist) Nil() bool {
	funcall.force()
	return funcall.result.Nil()
}

func (funcall *funcallBitlist) Bit() bool {
	funcall.force()
	return funcall.result.Bit()
}

func (funcall *funcallBitlist) Next() Bitlist {
	funcall.force()
	return funcall.result.Next()
}

func EvalFn(deflist []*Def, args []Bitlist) Bitlist {
	for _, def := range deflist {
		if result := matchEval(def, args); result != nil {
			return result
		}
	}
	return nil
}

func matchEval(def *Def, args []Bitlist) Bitlist {
	bindings := []Bitlist{}
	for i, arg := range args {
		match := def.Parameters[i].Match
		if match != nil {
			for !match.Nil() {
				if arg.Nil() || arg.Bit() != match.Bit() {
					return nil
				}
				arg = arg.Next()
				match = match.Next()
			}
		}
		if def.Parameters[i].Bound {
			bindings = append(bindings, arg)
		} else if !def.Parameters[i].Wild && !arg.Nil() {
			return nil
		}
	}
	return def.Body.Eval(bindings)
}

func unparseBits(bits Bitlist, buf *bytes.Buffer) {
	for !bits.Nil() {
		if bits.Bit() {
			buf.WriteRune('1')
		} else {
			buf.WriteRune('0')
		}
		bits = bits.Next()
	}
}

func unparseParameters(parameters []Parameter, buf *bytes.Buffer) {
	for _, parameter := range parameters {
		if parameter.Match != nil {
			unparseBits(parameter.Match, buf)
		}
		switch {
		case parameter.Bound:
			buf.WriteString(parameter.Bind.Token)
			buf.WriteRune(' ')
		case parameter.Wild:
			buf.WriteString(". ")
		default:
			buf.WriteString("_ ")
		}
	}
}

func unparseExpr(expr Expr, buf *bytes.Buffer) {
	switch expr.(type) {
	case *LiteralExpr:
		buf.WriteRune(' ')
		unparseBits(expr.(*LiteralExpr).bits, buf)
		buf.WriteRune('_')
	case *ConcatExpr:
		unparseExpr(expr.(*ConcatExpr).first, buf)
		unparseExpr(expr.(*ConcatExpr).last, buf)
	case *BoundExpr:
		buf.WriteRune(' ')
		buf.WriteString(expr.(*BoundExpr).name.Token)
	case *FuncallExpr:
		buf.WriteRune(' ')
		buf.WriteString(expr.(*FuncallExpr).name.Token)
		for _, argExpr := range expr.(*FuncallExpr).argExprs {
			unparseExpr(argExpr, buf)
		}
	}
}

func Unparse(def *Def) string {
	var buf bytes.Buffer
	buf.WriteString(def.Name.Token)
	buf.WriteRune(' ')
	unparseParameters(def.Parameters, &buf)
	buf.WriteRune('=')
	unparseExpr(def.Body, &buf)
	buf.WriteRune('.')
	return buf.String()
}
