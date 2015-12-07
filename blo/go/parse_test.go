package main

import (
	"strings"
	"testing"
)

func testParse(t *testing.T, label, input, expectedErr string, expectedFuncs, expectedTypes []string) *Ast {
	tokens := make(chan Token)
	go func() {
		Tokenize(strings.NewReader(input), tokens)
		close(tokens)
	}()
	ast, err := Parse(tokens)
	if err == nil && expectedErr != "" {
		t.Errorf("%s: Failed to get expected error `%s'", label, expectedErr)
	} else if err != nil && expectedErr != err.Error() {
		t.Errorf("%s: Expected error `%s', got error `%s'", label, err.Error(), expectedErr)
	}
	if err != nil {
		return ast
	}
	if len(expectedFuncs) != len(ast.Funcs) {
		t.Errorf("%s: Expected %d func(s), got %d", label, len(expectedFuncs), len(ast.Funcs))
	}
	if len(expectedTypes) != len(ast.Types) {
		t.Errorf("%s: Expected %d type(s), got %d", label, len(expectedTypes), len(ast.Types))
	}
	for _, name := range expectedFuncs {
		if funcDecl, ok := ast.Funcs[name]; !ok {
			t.Errorf("%s: Did not find expected func `%s'", label, name)
		} else if funcDecl.Name != name {
			t.Errorf("%s: Function name mismatch `%s' != `%s'", label, funcDecl.Name, name)
		}
	}
	for _, name := range expectedTypes {
		if typeDecl, ok := ast.Types[name]; !ok {
			t.Errorf("%s: Did not find expected type `%s'", label, name)
		} else if typeDecl.Name != name {
			t.Errorf("%s: Type name mismatch `%s' != `%s'", label, typeDecl.Name, name)
		}
	}
	return ast
}

func checkTypeFields(t *testing.T, label string, typeDecl *Type, fields []*Var) {
	if len(typeDecl.Fields) != len(fields) {
		t.Errorf("%s: Expected %d field(s), got %d", label, len(fields), len(typeDecl.Fields))
	}
	for i, field := range typeDecl.Fields {
		if field.Name != fields[i].Name {
			t.Errorf("%s: Expected field name `%s', got `%s'", label, fields[i].Name, field.Name)
		}
		if field.TypeName != fields[i].TypeName {
			t.Errorf("%s: Expected field type name `%s', got `%s'", label, fields[i].TypeName, field.TypeName)
		}
	}
}

func checkFuncParams(t *testing.T, label string, funcDecl *Func, params []*Var, returnType string) {
	if len(funcDecl.Params) != len(params) {
		t.Errorf("%s: Expected %d param(s), got %d", label, len(params), len(funcDecl.Params))
	}
	for i, param := range funcDecl.Params {
		if param.Name != params[i].Name {
			t.Errorf("%s: Expected parameter name `%s', got `%s'", label, params[i].Name, param.Name)
		}
		if param.TypeName != params[i].TypeName {
			t.Errorf("%s: Expected parameter type name `%s', got `%s'", label, params[i].TypeName, param.TypeName)
		}
	}
	if returnType != funcDecl.TypeName {
		t.Errorf("%s: Expected return type name `%s', got `%s'", label, returnType, funcDecl.TypeName)
	}
}

func TestParse(t *testing.T) {
	ast := testParse(t, "single type", "type a {\n  a, b\n  c d\n}", "", []string{}, []string{"a"})
	checkTypeFields(t, "single type", ast.Types["a"], []*Var{
		&Var{Name: "a", TypeName: ""},
		&Var{Name: "b", TypeName: ""},
		&Var{Name: "c", TypeName: "d"},
	})
	ast = testParse(t, "import func", "import func &(a, b bit) bit\nimport func +(a, b bit) bit+carry\nimport func putByte(b byte)\nimport func getByte(b byte)", "", []string{"&", "+", "putByte", "getByte"}, []string{})
	checkFuncParams(t, "import func &", ast.Funcs["&"], []*Var{
		&Var{Name: "a", TypeName: "bit"},
		&Var{Name: "b", TypeName: "bit"},
	}, "bit")
	checkFuncParams(t, "import func +", ast.Funcs["+"], []*Var{
		&Var{Name: "a", TypeName: "bit"},
		&Var{Name: "b", TypeName: "bit"},
	}, "bit+carry")
	checkFuncParams(t, "import func putByte", ast.Funcs["putByte"], []*Var{
		&Var{Name: "b", TypeName: "byte"},
	}, "")
	checkFuncParams(t, "import func getByte", ast.Funcs["getByte"], []*Var{
		&Var{Name: "b", TypeName: "byte"},
	}, "")
	ast = testParse(t, "import func and type", "import func pause()\ntype a { a; b c; }\n", "", []string{"pause"}, []string{"a"})
	checkTypeFields(t, "import func and type", ast.Types["a"], []*Var{
		&Var{Name: "a", TypeName: ""},
		&Var{Name: "b", TypeName: "c"},
	})
	checkFuncParams(t, "import func and type", ast.Funcs["pause"], []*Var{}, "")
}
