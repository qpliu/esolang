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
	if t.Imported {
		opaqueIndex++
	}
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
	for _, field := range t.Fields {
		if field.Name == fieldName && field.Type == nil {
			return v.Bits[bitIndex]
		}
		bitIndex += field.Type.BitSize()
	}
	panic("type '" + t.Name + "' does not have bit field '" + fieldName + "'")
}

func (v *Value) SetBitField(fieldName string, t *Type, value bool) {
	bitIndex := 0
	for _, field := range t.Fields {
		if field.Name == fieldName && field.Type == nil {
			v.Bits[bitIndex] = value
			return
		}
		bitIndex += field.Type.BitSize()
	}
	panic("type '" + t.Name + "' does not have bit field '" + fieldName + "'")
}

type Scope map[string]*Value

func NewScope(funcDecl *Func, args []*Value) Scope {
	scope := Scope(make(map[string]*Value))
	for i, param := range funcDecl.Params {
		scope[param.Name] = args[i]
	}
	return scope
}
