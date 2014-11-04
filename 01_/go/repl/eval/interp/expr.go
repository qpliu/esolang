package interp

type Expr interface {
	Eval(result *Value, defs map[string]*Def, args []*Value) error
}

type literalExpr string

func (expr literalExpr) Eval(result *Value, defs map[string]*Def, args []*Value) error {
	str := string(expr)
	if len(str) == 0 || str[0] == '_' {
		result.val = vnil
		result.next = nil
		result.expr = nil
		result.args = nil
		return nil
	} else if str[0] == '0' {
		result.val = v0
	} else if str[0] == '1' {
		result.val = v1
	} else {
		panic("invalid literal:" + str)
	}
	result.next = &Value{
		val:  vthunk,
		next: nil,
		expr: literalExpr(str[1:]),
		args: args,
	}
	result.expr = nil
	result.args = nil
	return nil
}

type argExpr int

func (expr argExpr) Eval(result *Value, defs map[string]*Def, args []*Value) error {
	arg := args[int(expr)]
	result.val = arg.val
	result.next = arg.next
	result.expr = arg.expr
	result.args = arg.args
	return nil
}

type concatExpr [2]Expr

func (expr concatExpr) Eval(result *Value, defs map[string]*Def, args []*Value) error {
	if err := expr[0].Eval(result, defs, args); err != nil {
		return err
	}
	switch result.val {
	case v0, v1:
		result.next.expr = concatExpr{result.next.expr, expr[1]}
	case vnil:
		result.val = vthunk
		result.expr = expr[1]
	case vthunk:
		result.expr = concatExpr{result.expr, expr[1]}
	default:
		panic("unreachable")
	}
	return nil
}
