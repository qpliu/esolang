package interp

import "errors"

type Def struct {
	name     string
	arity    uint
	bodies   [][2][]string
	compiled []compiledDef
}

func NewDef(name string, parameters, body []string) *Def {
	return &Def{
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

func (def *Def) compile(defs map[string]*Def) error {
	if len(def.compiled) == len(def.bodies) {
		return nil
	}
	return errors.New("not implemented")
}

func arity(parameters []string) uint {
	return uint(len(compileParams(parameters)))
}

type compiledDef struct {
	params     []param
	body       Expr
	compileErr error
}

type paramType int

const (
	paramBind   paramType = iota
	paramIgnore paramType = iota
	paramNil    paramType = iota
)

type param struct {
	match []bool
	ptype paramType
	bind  string
}

func compileParams(tokens []string) []param {
	var params []param
	var match []bool
	for _, token := range tokens {
		literal, terminated := isLiteralTerminated(token)
		if literal {
			if match != nil {
				panic("tokenizer yielded multiple literals")
			}
			for _, c := range token {
				switch c {
				case '0':
					match = append(match, false)
				case '1':
					match = append(match, true)
				case '_':
				default:
					panic("tokenizer yielded invalid literal")
				}
			}
			if terminated {
				params = append(params, param{match: match, ptype: paramNil})
				match = nil
			}
		} else if token == "." {
			params = append(params, param{match: match, ptype: paramIgnore})
			match = nil
		} else {
			params = append(params, param{match: match, ptype: paramBind, bind: token})
			match = nil
		}
	}
	if match != nil {
		params = append(params, param{match: match, ptype: paramIgnore})
	}
	return params
}

func CompileExpr(tokens []string, defs map[string]*Def) (Expr, error) {
	return compileExpr(tokens, defs, nil)
}

func compileExpr(tokens []string, defs map[string]*Def, params []param) (Expr, error) {
	bindings := make(map[string]uint)
	i := uint(0)
	for _, param := range params {
		if param.ptype == paramBind {
			if _, ok := bindings[param.bind]; ok {
				return nil, errors.New("duplicate parameter:" + param.bind)
			}
			bindings[param.bind] = i
			i++
		}
	}
	var stack *compileStack = nil
	for _, token := range tokens {
		var expr Expr = nil
		if isLiteral, _ := isLiteralTerminated(token); isLiteral {
			expr = literalExpr(token)
		} else if index, ok := bindings[token]; ok {
			expr = argExpr(index)
		} else if def, ok := defs[token]; ok && def.arity == 0 {
			expr = funcallExpr{def: def, args: nil}
		}
		if expr != nil {
			stack = stack.push()
			stack.expr = expr
			for stack.argc > 0 && stack.argi+1 >= stack.argc {
				args := make([]Expr, stack.argc)
				for {
					if stack.call != nil {
						panic("stack.call != nil")
					}
					if stack.argi != 0 && stack.previous.argi+1 != stack.argi {
						panic("stack.argi != 0 && stack.previous.argi + 1 != stack.argi")
					}
					args[stack.argi] = stack.expr
					if stack.argi != 0 {
						stack = stack.previous
						continue
					}
					stack = stack.previous
					if stack.call == nil || stack.expr != nil || stack.call.arity != uint(len(args)) {
						panic("stack.call == nil || stack.expr != nil || stack.call.arity != uint(len(args))")
					}
					stack.expr = funcallExpr{def: stack.call, args: args}
					stack.call = nil
					break
				}
			}
		} else if def, ok := defs[token]; ok {
			if def.arity == 0 {
				panic("def.arity == 0")
			}
			stack = stack.push()
			stack.call = def
		} else {
			return nil, errors.New("undefined symbol:" + token)
		}
	}
	if stack == nil {
		return literalExpr(""), nil
	} else if stack.argc != 0 || stack.call != nil {
		return nil, errors.New("not enough arguments to a function")
	}
	if stack.expr == nil {
		panic("stack.expr == nil")
	}
	appendExpr := stack.expr
	for stack = stack.previous; stack != nil; stack = stack.previous {
		if stack.expr == nil || stack.call != nil || stack.argc != 0 {
			panic("stack.expr == nil || stack.call != nil || stack.argc != 0")
		}
		appendExpr = concatExpr{stack.expr, appendExpr}
	}
	return appendExpr, nil
}

type compileStack struct {
	previous   *compileStack
	call       *Def
	expr       Expr
	argc, argi uint
}

func (stack *compileStack) push() *compileStack {
	if stack == nil {
		return &compileStack{}
	} else if stack.call != nil && stack.call.arity > 0 {
		return &compileStack{previous: stack, argc: stack.call.arity}
	} else if stack.argc == 0 {
		return &compileStack{previous: stack}
	}
	return &compileStack{previous: stack, argc: stack.argc, argi: stack.argi + 1}
}

func EvalExpr(defs map[string]*Def, expr Expr, args []*Value) *Value {
	return &Value{vthunk, nil, expr, args}
}

func (def *Def) Show() []string {
	result := make([]string, 0, len(def.bodies))
	for _, body := range def.bodies {
		s := def.name
		addSpace := true
		for _, token := range body[0] {
			if addSpace {
				s += " "
			}
			s += token
			literal, terminated := isLiteralTerminated(token)
			addSpace = !literal || terminated
		}
		s += " ="
		for _, token := range body[1] {
			s += " " + token
		}
		result = append(result, s+".")
	}
	return result
}

func isLiteralTerminated(token string) (bool, bool) {
	if token == "_" {
		return true, true
	} else if token[0] == '0' || token[0] == '1' {
		return true, token[len(token)-1] == '_'
	} else {
		return false, false
	}
}
