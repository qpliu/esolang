package ast

import (
	"../bitlist"
	"../tokenize"
	"bytes"
	"fmt"
)

type Parameter struct {
	MatchToken tokenize.Token
	Match      bitlist.Bitlist
	Bound      bool
	Bind       tokenize.Token
	Wild       bool
}

type Def struct {
	Name       tokenize.Token
	Parameters []Parameter
	Body       Expr
}

type Expr interface {
	Eval(bindings []bitlist.Bitlist) bitlist.Bitlist
}

type literalExpr struct {
	bits bitlist.Bitlist
}

func (expr *literalExpr) Eval(bindings []bitlist.Bitlist) bitlist.Bitlist {
	return expr.bits
}

type concatExpr struct {
	first, last Expr
}

func (expr *concatExpr) Eval(bindings []bitlist.Bitlist) bitlist.Bitlist {
	return &concatBitlist{bindings: bindings, expr: expr}
}

type concatBitlist struct {
	bindings    []bitlist.Bitlist
	expr        *concatExpr
	first, last bitlist.Bitlist
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

func (concat *concatBitlist) Next() bitlist.Bitlist {
	concat.force()
	if concat.first.Nil() {
		return concat.last.Next()
	}
	if concat.next == nil {
		concat.next = &concatBitlist{bindings: concat.bindings, expr: concat.expr, first: concat.first.Next()}
	}
	return concat.next
}

type boundExpr struct {
	name  tokenize.Token
	index int
}

func (expr *boundExpr) Eval(bindings []bitlist.Bitlist) bitlist.Bitlist {
	return bindings[expr.index]
}

type funcallExpr struct {
	name     tokenize.Token
	defs     []*Def
	argExprs []Expr
}

func (expr *funcallExpr) Eval(bindings []bitlist.Bitlist) bitlist.Bitlist {
	return &funcallBitlist{bindings: bindings, expr: expr}
}

type funcallBitlist struct {
	bindings []bitlist.Bitlist
	expr     *funcallExpr
	result   bitlist.Bitlist
}

func (funcall *funcallBitlist) force() {
	if funcall.result != nil {
		return
	}
	args := make([]bitlist.Bitlist, 0, len(funcall.expr.argExprs))
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

func (funcall *funcallBitlist) Next() bitlist.Bitlist {
	funcall.force()
	return funcall.result.Next()
}

func EvalFn(deflist []*Def, args []bitlist.Bitlist) bitlist.Bitlist {
	for _, def := range deflist {
		if result := matchEval(def, args); result != nil {
			return result
		}
	}
	return nil
}

func matchEval(def *Def, args []bitlist.Bitlist) bitlist.Bitlist {
	bindings := []bitlist.Bitlist{}
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

func unparseBits(bits bitlist.Bitlist, buf *bytes.Buffer) {
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
	switch e := expr.(type) {
	case *literalExpr:
		buf.WriteRune(' ')
		unparseBits(e.bits, buf)
		buf.WriteRune('_')
	case *concatExpr:
		unparseExpr(e.first, buf)
		unparseExpr(e.last, buf)
	case *boundExpr:
		buf.WriteRune(' ')
		buf.WriteString(e.name.Token)
	case *funcallExpr:
		buf.WriteRune(' ')
		buf.WriteString(e.name.Token)
		for _, argExpr := range e.argExprs {
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
