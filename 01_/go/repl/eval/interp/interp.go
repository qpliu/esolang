package interp

import "errors"

type Def struct {
	name     string
	arity    int
	bodies   [][2][]string
	compiled []compiledDef
}

func NewDef(name string, parameters, body []string) Def {
	return Def{
		name:   name,
		arity:  arity(parameters),
		bodies: [][2][]string{[2][]string{parameters, body}},
	}
}

func (def *Def) Add(parameters, body []string) error {
	if def.arity != arity(parameters) {
		return errors.New("arity mismatch")
	}
	def.bodies = append(def.bodies, [2][]string{parameters, body})
	return nil
}

func (def *Def) ClearCompilation() {
	def.compiled = nil
}

func arity(parameters []string) int {
	arity := 0
	matchPrefix := false
	for _, token := range parameters {
		if (token[0] == '0' || token[0] == '1') && token[len(token)-1] != '_' {
			matchPrefix = true
		} else {
			matchPrefix = false
			arity++
		}
	}
	if matchPrefix {
		arity++
	}
	return arity
}

type compiledDef struct {
	// header
	body Expr
}

type Expr interface {
}

type valType int

const (
	v0     valType = iota
	v1     valType = iota
	vnil   valType = iota
	vthunk valType = iota
)

type Value struct {
	val  valType
	next *Value
	expr Expr
	args []*Value
}

func (v *Value) Force(defs map[string]Def) (bool, *Value, error) {
	switch v.val {
	case v0, v1, vnil:
		return v.val == v1, v.next, nil
	case vthunk:
		return false, nil, errors.New("not implemented")
	default:
		panic("unreachable")
	}
}

func CompileExpr(tokens []string, defs map[string]Def) (Expr, error) {
	return nil, errors.New("not implemented")
}

func EvalExpr(defs map[string]Def, expr Expr, args []*Value) *Value {
	return &Value{vthunk, nil, expr, args}
}
