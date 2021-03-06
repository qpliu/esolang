package ast

import (
	"../bitlist"
	"../tokenize"
	"errors"
	"fmt"
)

type def1 struct {
	name       tokenize.Token
	parameters []tokenize.Token
	body       []tokenize.Token
}

func parse1(tokens <-chan tokenize.Token) (map[string][]*def1, error) {
	var name tokenize.Token
	inDef := false
	inBody := false
	defs := make(map[string][]*def1)
	var def *def1 = nil
	for token := range tokens {
		switch {
		case !inDef:
			if token.IsName() {
				name = token
				def = new(def1)
				def.name = token
				inDef = true
				inBody = false
			} else {
				return defs, errors.New(fmt.Sprintf("%s name expected", token.Location()))
			}
		case token.Token == "=":
			if inBody {
				return defs, errors.New(fmt.Sprintf("%s unexpected =", token.Location()))
			} else {
				inBody = true
			}
		case token.Token == ".":
			if inBody {
				def.body = append(def.body, token)
				defs[name.Token] = append(defs[name.Token], def)
				inDef = false
				inBody = false
				def = nil
				break
			}
			fallthrough
		default:
			if inBody {
				def.body = append(def.body, token)
			} else {
				def.parameters = append(def.parameters, token)
			}
		}
	}
	if inDef {
		return defs, errors.New(fmt.Sprintf("%s incomplete body for %s", name.Location(), name.Token))
	}
	return defs, nil
}

type def2 struct {
	name       tokenize.Token
	parameters []Parameter
	body       []tokenize.Token
}

func parse2(def1s map[string][]*def1) (map[string][]*def2, error) {
	def2s := make(map[string][]*def2)
	for name, def1list := range def1s {
		def2list := make([]*def2, 0, len(def1list))
		for _, def1item := range def1list {
			parameters, err := parseParameters(def1item.parameters)
			if err != nil {
				return def2s, err
			}
			def2item := &def2{def1item.name, parameters, def1item.body}
			def2list = append(def2list, def2item)
			if len(def2item.parameters) != len(def2list[0].parameters) {
				return def2s, errors.New(fmt.Sprintf("%s %d-ary def does not match %d-ary def at %s",
					def2item.name.Location(),
					len(def2item.parameters),
					len(def2list[0].parameters),
					def2list[0].name.Location()))
			}
		}
		def2s[name] = def2list
	}
	return def2s, nil
}

func parseBits(token tokenize.Token) bitlist.Bitlist {
	if !token.IsLiteral() {
		return nil
	}
	len := len(token.Token)
	if token.IsTerminatedLiteral() {
		len--
	}
	bits := make([]bool, len)
	for i := range bits {
		bits[i] = token.Token[i] == '1'
	}
	return bitlist.NewLiteralBitlist(bits)
}

func parseParameters(tokens []tokenize.Token) ([]Parameter, error) {
	parameters := []Parameter{}
	if tokens == nil {
		return parameters, nil
	}
	pendingMatch := false
	for _, token := range tokens {
		switch {
		case token.IsTerminatedLiteral():
			parameters = append(parameters, Parameter{MatchToken: token, Match: parseBits(token), Bound: false, Wild: false})
			pendingMatch = false
		case token.IsLiteral():
			parameters = append(parameters, Parameter{MatchToken: token, Match: parseBits(token)})
			pendingMatch = true
		default:
			if pendingMatch {
				pendingMatch = false
			} else {
				parameters = append(parameters, Parameter{})
			}
			parameter := &parameters[len(parameters)-1]
			switch {
			case token.IsName():
				for _, p := range parameters {
					if p.Bound && p.Bind.Token == token.Token {
						return parameters, errors.New(fmt.Sprintf("%s duplicate parameter name: %s", token.Location(), token.Token))
					}
				}
				parameter.Bound = true
				parameter.Bind = token
				parameter.Wild = false
			case token.Token == ".":
				parameter.Bound = false
				parameter.Wild = true
			default:
				return parameters, errors.New(fmt.Sprintf("%s unexpected token", token.Location()))
			}
		}
	}
	if pendingMatch {
		parameter := &parameters[len(parameters)-1]
		parameter.Bound = false
		parameter.Wild = true
	}
	return parameters, nil
}

func parse3(def2s map[string][]*def2) (map[string][]*Def, error) {
	arities := make(map[string]int)
	for name, def2list := range def2s {
		arities[name] = len(def2list[0].parameters)
	}
	defs := make(map[string][]*Def)
	for name, def2list := range def2s {
		deflist := make([]*Def, 0, len(def2list))
		for _, def2item := range def2list {
			defitem, err := parseDef(arities, def2item)
			if err != nil {
				return defs, err
			}
			deflist = append(deflist, defitem)
		}
		defs[name] = deflist
	}
	return defs, nil
}

func parseDef(arities map[string]int, def2item *def2) (*Def, error) {
	bound := []string{}
	for _, parameter := range def2item.parameters {
		if parameter.Bound {
			bound = append(bound, parameter.Bind.Token)
		}
	}
	body, err := parseBody(def2item.name, bound, arities, def2item.body)
	return &Def{def2item.name, def2item.parameters, body}, err
}

func parseBody(name tokenize.Token, bound []string, arities map[string]int, tokens []tokenize.Token) (Expr, error) {
	if len(tokens) == 0 || (len(tokens) == 1 && tokens[0].Token == ".") {
		return &literalExpr{bitlist.NilBitlist}, nil
	}
	expr, tokens, err := parseExpr(name, bound, arities, tokens)
	if err != nil {
		return expr, err
	}
	if len(tokens) == 0 || (len(tokens) == 1 && tokens[0].Token == ".") {
		return expr, nil
	}
	rest, err := parseBody(name, bound, arities, tokens)
	return &concatExpr{first: expr, last: rest}, err
}

func parseExpr(name tokenize.Token, bound []string, arities map[string]int, tokens []tokenize.Token) (Expr, []tokenize.Token, error) {
	switch {
	case len(tokens) == 0:
		return nil, tokens, errors.New(fmt.Sprintf("%s unexpected end of def %s", name.Location(), name.Token))
	case tokens[0].Token == "." || tokens[0].Token == "=":
		return nil, tokens[1:], errors.New(fmt.Sprintf("%s unexpected end of def %s", tokens[0].Location(), name.Token))
	case tokens[0].IsLiteral():
		return &literalExpr{parseBits(tokens[0])}, tokens[1:], nil
	}
	for index, parameter := range bound {
		if parameter == tokens[0].Token {
			return &boundExpr{name: tokens[0], index: index}, tokens[1:], nil
		}
	}
	arity, defined := arities[tokens[0].Token]
	if !defined {
		return nil, tokens[1:], errors.New(fmt.Sprintf("%s undefined %s", tokens[0].Location(), tokens[0].Token))
	}
	argExprs := make([]Expr, 0, arity)
	argTokens := tokens[1:]
	for i := 0; i < arity; i++ {
		argExpr, remainingTokens, err := parseExpr(name, bound, arities, argTokens)
		argTokens = remainingTokens
		if err != nil {
			return nil, argTokens, err
		}
		argExprs = append(argExprs, argExpr)
	}
	return &funcallExpr{name: tokens[0], argExprs: argExprs}, argTokens, nil
}

func initFuncalls(expr Expr, defs map[string][]*Def) {
	switch e := expr.(type) {
	case *concatExpr:
		initFuncalls(e.first, defs)
		initFuncalls(e.last, defs)
	case *funcallExpr:
		e.defs = defs[e.name.Token]
		for _, argExpr := range e.argExprs {
			initFuncalls(argExpr, defs)
		}
	}
}

func Parse(tokens <-chan tokenize.Token) (map[string][]*Def, error) {
	def1s, err := parse1(tokens)
	if err != nil {
		return nil, err
	}
	def2s, err := parse2(def1s)
	if err != nil {
		return nil, err
	}
	defs, err := parse3(def2s)
	if err != nil {
		return defs, err
	}
	for _, deflist := range defs {
		for _, def := range deflist {
			initFuncalls(def.Body, defs)
		}
	}
	return defs, nil
}
