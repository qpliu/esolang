package interp

type valType int

const (
	v0     valType = iota
	v1     valType = iota
	vnil   valType = iota
	vthunk valType = iota
)

type Value struct {
	val      valType
	next     *Value
	expr     Expr
	bindings []*Value
}

func (v *Value) Force(defs map[string]*Def) (bool, *Value, error) {
	for {
		switch v.val {
		case v0, v1, vnil:
			return v.val == v1, v.next, nil
		case vthunk:
			if err := v.expr.Eval(v, defs); err != nil {
				return false, nil, err
			}
		default:
			panic("unreachable")
		}
	}
}
