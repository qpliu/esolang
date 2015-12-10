package main

import (
	"strings"
	"testing"
)

func testAnnotate(t *testing.T, label, input, expectedErr string) *Ast {
	tokens := make(chan Token)
	go func() {
		Tokenize("(stdin)", strings.NewReader(input), tokens)
		close(tokens)
	}()
	if ast, err := Parse(tokens); err != nil {
		t.Errorf("%s: Unexpected parse error: %s", label, err.Error())
	} else if err := ast.Annotate(); err != nil {
		if expectedErr != err.Error() {
			t.Errorf("%s: Expected error `%s', got error `%s'", label, expectedErr, err.Error())
		}
	} else if expectedErr != "" {
		t.Errorf("%s: Failed to get expected error `%s'", label, expectedErr)
	} else {
		return ast
	}
	return nil
}

func checkAnnotatedType(t *testing.T, label string, typeDecl *Type, expectedFields []*Var, expectedBitSize, expectedOpaqueSize int) {
	if len(typeDecl.Fields) != len(expectedFields) {
		t.Errorf("%s: Expected %d fields, got %d", len(expectedFields), len(typeDecl.Fields))
	}
	for i, field := range typeDecl.Fields {
		if field.Name != expectedFields[i].Name || field.TypeName != expectedFields[i].TypeName || field.Type != expectedFields[i].Type {
			t.Errorf("%s: Expected field %d `%s %s', got `%s %s'", label, i, expectedFields[i].Name, expectedFields[i].TypeName, field.Name, field.TypeName)
		}
	}
	if expectedBitSize != typeDecl.BitSize() {
		t.Errorf("%s: Expected bit size %d, got %d", expectedBitSize, typeDecl.BitSize())
	}
	if expectedOpaqueSize != typeDecl.OpaqueSize() {
		t.Errorf("%s: Expected opaque size %d, got %d", expectedOpaqueSize, typeDecl.OpaqueSize())
	}
}

func checkAnnotatedFuncDecl(t *testing.T, label string, funcDecl *Func, expectedType *Type, expectedParams []*Var) {
	if funcDecl.Type != expectedType {
		if expectedType == nil {
			t.Errorf("%s: Function `%s' expected no return type, got `%s'", funcDecl.Type.Name)
		} else if funcDecl.Type == nil {
			t.Errorf("%s: Function `%s' expected to return type `%s', got no return type", expectedType.Name)
		} else {
			t.Errorf("%s: Function `%s' expected to return type `%s', got `%s'", expectedType.Name, funcDecl.Type.Name)
		}
	}
	if len(funcDecl.Params) != len(expectedParams) {
		t.Errorf("%s: Expected %d parameters, got %d", len(expectedParams), len(funcDecl.Params))
	}
	for i, param := range funcDecl.Params {
		if param.Name != expectedParams[i].Name || param.TypeName != expectedParams[i].TypeName || param.Type != expectedParams[i].Type {
			t.Errorf("%s: Expected field %d `%s %s', got `%s %s'", label, i, expectedParams[i].Name, expectedParams[i].TypeName, param.Name, param.TypeName)
		}
	}
}

func TestAnnotate(t *testing.T) {
	ast := testAnnotate(t, "single type", `type a { a, b }`, "")
	checkAnnotatedType(t, "single type", ast.Types["a"], []*Var{
		&Var{Name: "a", TypeName: "", Type: nil},
		&Var{Name: "b", TypeName: "", Type: nil},
	}, 2, 0)

	testAnnotate(t, "single recursive type", `type a { a a }`, "(stdin):1:1: Type contains recursive field of type 'a'")

	ast = testAnnotate(t, "embedded type", `type a { a } type b { 1, 2 a }`, "")
	checkAnnotatedType(t, "embedded type:a", ast.Types["a"], []*Var{
		&Var{Name: "a", TypeName: "", Type: nil},
	}, 1, 0)
	checkAnnotatedType(t, "embedded type:b", ast.Types["b"], []*Var{
		&Var{Name: "1", TypeName: "a", Type: ast.Types["a"]},
		&Var{Name: "2", TypeName: "a", Type: ast.Types["a"]},
	}, 2, 0)

	ast = testAnnotate(t, "func no params", `func main() {}`, "")
	checkAnnotatedFuncDecl(t, "func no params:main", ast.Funcs["main"], nil, []*Var{})

	ast = testAnnotate(t, "func with type", `
type a {
  a
}
func a(a a) a {
}
`, "")
	checkAnnotatedFuncDecl(t, "func with type", ast.Funcs["a"], ast.Types["a"], []*Var{&Var{Name: "a", TypeName: "a", Type: ast.Types["a"]}})
}
