package interp

import "errors"

type Def struct {
	name     string
	arity    int
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

func arity(parameters []string) int {
	return len(compileParams(parameters))
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
	return nil, errors.New("not implemented")
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
