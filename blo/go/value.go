package main

type Value struct {
	Bits   []bool
	Opaque []interface{}
}

func NewValue(t *Type) *Value {
	return &Value{
		Bits:   make([]bool, t.BitSize()),
		Opaque: make([]interface{}, t.OpaqueSize()),
	}
}

func (v *Value) Field(fieldName string, t *Type) *Value {
	bitIndex := 0
	opaqueIndex := 0
	for _, field := range t.Fields {
		if field.Name == fieldName && field.Type != nil {
			return &Value{
				Bits:   v.Bits[bitIndex : bitIndex+field.Type.BitSize()],
				Opaque: v.Opaque[opaqueIndex : opaqueIndex+field.Type.OpaqueSize()],
			}
		}
		bitIndex += field.Type.BitSize()
		opaqueIndex += field.Type.OpaqueSize()
	}
	panic("type '" + t.Name + "' does not have field '" + fieldName + "'")
}

func (v *Value) BitField(fieldName string, t *Type) bool {
	bitIndex := 0
	opaqueIndex := 0
	for _, field := range t.Fields {
		if field.Name == fieldName && field.Type == nil {
			return v.Bits[bitIndex]
		}
		bitIndex += field.Type.BitSize()
		opaqueIndex += field.Type.OpaqueSize()
	}
	panic("type '" + t.Name + "' does not have bit field '" + fieldName + "'")
}

type Scope struct {
	Values map[string]*Value
	Parent *Scope
}

func NewScope(funcDecl *Func, args []*Value) *Scope {
	scope := &Scope{Values: make(map[string]*Value)}
	for i, param := range funcDecl.Params {
		scope.Values[param.Name] = args[i]
	}
	return scope
}

func NestedScope(parent *Scope) *Scope {
	return &Scope{Values: make(map[string]*Value), Parent: parent}
}

func (s *Scope) Lookup(name string) (*Value, bool) {
	if value, ok := s.Values[name]; ok {
		return value, true
	} else if s.Parent != nil {
		return s.Parent.Lookup(name)
	} else {
		return nil, false
	}
}

func (s *Scope) Add(name string) {
	s.Values[name] = nil
}

func (s *Scope) Set(name string, value *Value) {
	if _, ok := s.Values[name]; ok {
		s.Values[name] = value
	} else {
		s.Parent.Set(name, value)
	}
}
