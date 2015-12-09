package main

import (
	"errors"
)

func (ast *Ast) Annotate() error {
	if err := annotateTypeDecls(ast); err != nil {
		return err
	}
	if err := annotateFuncs(ast); err != nil {
		return err
	}
	return nil
}

func annotateTypeDecls(ast *Ast) error {
	for typeName, typeDecl := range ast.Types {
		for _, field := range typeDecl.Fields {
			if field.TypeName == "" {
			} else if fieldType, ok := ast.Types[field.TypeName]; !ok {
				return errors.New(typeDecl.Location.String() + ": Unknown type '" + field.TypeName + "' for field '" + field.Name + "' in type `" + typeName + "'")
			} else {
				field.Type = fieldType
			}
		}
	}
	typeSet := make(map[string]bool)
	for typeName, typeDecl := range ast.Types {
		typeSet[typeName] = true
		if err := checkRecursiveTypes(typeDecl, typeDecl, typeSet); err != nil {
			return err
		}
		delete(typeSet, typeName)
	}
	return nil
}

func checkRecursiveTypes(baseTypeDecl, typeDecl *Type, typeSet map[string]bool) error {
	for _, field := range typeDecl.Fields {
		if field.TypeName == "" {
		} else if _, ok := typeSet[field.TypeName]; ok {
			return errors.New(baseTypeDecl.Location.String() + ": Type contains recursive field of type '" + field.TypeName + "'")
		} else {
			typeSet[field.TypeName] = true
			if err := checkRecursiveTypes(baseTypeDecl, field.Type, typeSet); err != nil {
				return err
			}
			delete(typeSet, field.TypeName)
		}
	}
	return nil
}

func annotateFuncs(ast *Ast) error {
	//... panic("Not yet implemented") //...
	return nil
}
