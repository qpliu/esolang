package main

import (
	"errors"
)

type node struct {
	edges []*node
}

type vrbl struct {
	node *node
}

type scope struct {
	module string
	vrbls  map[string]*vrbl
}

func link(modules []*program) (map[string]func([]*vrbl), error) {
	return nil, errors.New("Not implemented")
}

func linkRoutine(library, name string, parameters []string, statements []statement) func([]*vrbl) {
	return func(args []*vrbl) {
		panic("Not implemented")
	}
}

func runtime_IO_READBYTE(args []*vrbl) {
	panic("Not implemented")
}

func runtime_IO_WRITEBYTE(args []*vrbl) {
	panic("Not implemented")
}

func (scope *scope) getVrbl(name string) *vrbl {
	v, ok := scope.vrbls[name]
	if ok {
		return v
	}
	v = &vrbl{&node{}}
	scope.vrbls[name] = v
	return v
}

func hasEdge(v1, v2 *vrbl) bool {
	for _, node := range v1.node.edges {
		if node == v2.node {
			return true
		}
	}
	return false
}

func makeEdge(v1, v2 *vrbl) {
	if hasEdge(v1, v2) {
		return
	}
	v1.node.edges = append(v1.node.edges, v2.node)
}

func removeEdge(v1, v2 *vrbl) {
	for i, node := range v1.node.edges {
		if node == v2.node {
			v1.node.edges[i] = v1.node.edges[len(v1.node.edges)-1]
			v1.node.edges = v1.node.edges[:len(v1.node.edges)-1]
			return
		}
	}
}
