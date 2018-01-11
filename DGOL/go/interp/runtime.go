package main

import (
	"errors"
	"os"
)

type node struct {
	edges []*node
}

type vrbl struct {
	node *node
}

type scope struct {
	moduleName string
	vrbls      map[string]*vrbl
}

func link(modules []*module, stdlibs map[string]map[string]func([]*vrbl)) (func(), error) {
	var main func([]*vrbl)
	routines := make(map[string]func([]*vrbl))
	public := make(map[string]bool)
	moduleNames := make(map[string]bool)
	for _, module := range modules {
		moduleName := module.name
		if len(module.library) == 0 {
			if main != nil {
				return nil, errors.New("MULTIPLE PROGRAM MODULES")
			} else {
				moduleName = ""
				main = linkRoutine(moduleName, nil, module.statements, routines)
			}
		} else if moduleNames[moduleName] {
			return nil, errors.New("DUPLICATE LIBRARY NAME: " + module.name)
		}
		for _, subroutine := range module.subroutines {
			routines[moduleName+"."+subroutine.name] = linkRoutine(moduleName, subroutine.parameters, subroutine.statements, routines)
		}
		for _, subroutine := range module.library {
			public[moduleName+"."+subroutine] = true
		}
		moduleNames[moduleName] = true
	}

	if main == nil {
		return nil, errors.New("NO PROGRAM MODULE")
	}

	if stdlibs != nil {
		for moduleName, stdlib := range stdlibs {
			if !moduleNames[moduleName] {
				moduleNames[moduleName] = true
				for routineName, routine := range stdlib {
					routines[moduleName+"."+routineName] = routine
					public[moduleName+"."+routineName] = true
				}
			}
		}
	}

	linkerCheckCalls := func(statements []statement) error {
		for _, stmt := range statements {
			if stmt.stmtType == stmtCall && stmt.parameters[0] != "" && !public[stmt.parameters[0]+"."+stmt.parameters[1]] {
				return errors.New("UNRESOLVED CALL: " + stmt.parameters[0] + "." + stmt.parameters[1])
			}
		}
		return nil
	}
	for _, module := range modules {
		if err := linkerCheckCalls(module.statements); err != nil {
			return nil, err
		}
		for _, subroutine := range module.subroutines {
			if err := linkerCheckCalls(subroutine.statements); err != nil {
				return nil, err
			}
		}
	}

	return func() {
		main(nil)
	}, nil
}

func linkRoutine(moduleName string, parameters []string, statements []statement, routines map[string]func([]*vrbl)) func([]*vrbl) {
	return func(args []*vrbl) {
		scope := newScope(moduleName)
		for i, param := range parameters {
			if i < len(args) {
				scope.vrbls[param] = args[i]
			}
		}
		for _, statement := range statements {
			exitOrReturn := executeStatement(statement, scope, routines)
			if exitOrReturn != nil {
				break
			}
		}
	}
}

type exitOrReturn struct {
	label string
}

func executeStatement(stmt statement, scope *scope, routines map[string]func([]*vrbl)) *exitOrReturn {
	switch stmt.stmtType {
	case stmtLetEq:
		scope.setVrbl(stmt.parameters[0], scope.getVrbl(stmt.parameters[1]))
	case stmtLetMkEdge:
		makeEdge(scope.getVrbl(stmt.parameters[0]), scope.getVrbl(stmt.parameters[1]))
	case stmtLetRmEdge:
		removeEdge(scope.getVrbl(stmt.parameters[0]), scope.getVrbl(stmt.parameters[1]))
	case stmtIf:
		var body []statement
	stmts:
		for _, st := range stmt.statements {
			switch st.stmtType {
			case stmtIfEq:
				if scope.getVrbl(st.parameters[0]).node == scope.getVrbl(st.parameters[1]).node {
					body = st.statements
					break stmts
				}
			case stmtIfEdge:
				if hasEdge(scope.getVrbl(st.parameters[0]), scope.getVrbl(st.parameters[1])) {
					body = st.statements
					break stmts
				}
			case stmtElse:
				body = st.statements
				break stmts
			default:
				panic("Unrecognized statement type")
			}
		}
		for _, st := range body {
			if exitOrReturn := executeStatement(st, scope, routines); exitOrReturn != nil {
				return exitOrReturn
			}
		}
	case stmtDoLoop:
		for {
			for _, st := range stmt.statements {
				if exitOrReturn := executeStatement(st, scope, routines); exitOrReturn == nil {
					continue
				} else if exitOrReturn.label == stmt.parameters[0] {
					return nil
				} else {
					return exitOrReturn
				}
			}
		}
	case stmtDoEdges:
		edges := append([]*node{}, scope.getVrbl(stmt.parameters[1]).node.edges...)
		for _, edge := range edges {
			scope.setVrbl(stmt.parameters[0], &vrbl{node: edge})
			for _, st := range stmt.statements {
				if exitOrReturn := executeStatement(st, scope, routines); exitOrReturn == nil {
					continue
				} else if exitOrReturn.label == stmt.parameters[0] {
					return nil
				} else {
					return exitOrReturn
				}
			}
		}
	case stmtCall:
		var callArgs []*vrbl
		for _, param := range stmt.parameters[2:] {
			callArgs = append(callArgs, scope.getVrbl(param))
		}
		routineName := stmt.parameters[0] + "." + stmt.parameters[1]
		if stmt.parameters[0] == "" {
			routineName = scope.moduleName + routineName
		}
		routines[routineName](callArgs)
	case stmtReturn:
		return &exitOrReturn{""}
	case stmtExit:
		return &exitOrReturn{stmt.parameters[0]}
	default:
		panic("Unrecognized statement type")
	}
	return nil
}

func makeStdlibs() map[string]map[string]func([]*vrbl) {
	stdlibs := make(map[string]map[string]func([]*vrbl))
	iolib := make(map[string]func([]*vrbl))
	stdlibs["IO"] = iolib
	iolib["READBYTE"] = runtime_IO_READBYTE
	iolib["WRITEBYTE"] = runtime_IO_WRITEBYTE
	return stdlibs
}

func runtime_IO_READBYTE(args []*vrbl) {
	buf := []byte{0}
	_, err := os.Stdin.Read(buf)
	if err != nil {
		if len(args) > 1 {
			makeEdge(args[0], args[1])
		}
		return
	}
	if len(args) > 1 {
		removeEdge(args[0], args[1])
	}
	if len(args) > 2 {
		if buf[0]&1 != 0 {
			makeEdge(args[0], args[2])
		} else {
			removeEdge(args[0], args[2])
		}
	}
	if len(args) > 3 {
		if buf[0]&2 != 0 {
			makeEdge(args[0], args[3])
		} else {
			removeEdge(args[0], args[3])
		}
	}
	if len(args) > 4 {
		if buf[0]&4 != 0 {
			makeEdge(args[0], args[4])
		} else {
			removeEdge(args[0], args[4])
		}
	}
	if len(args) > 5 {
		if buf[0]&8 != 0 {
			makeEdge(args[0], args[5])
		} else {
			removeEdge(args[0], args[5])
		}
	}
	if len(args) > 6 {
		if buf[0]&16 != 0 {
			makeEdge(args[0], args[6])
		} else {
			removeEdge(args[0], args[6])
		}
	}
	if len(args) > 7 {
		if buf[0]&32 != 0 {
			makeEdge(args[0], args[7])
		} else {
			removeEdge(args[0], args[7])
		}
	}
	if len(args) > 8 {
		if buf[0]&64 != 0 {
			makeEdge(args[0], args[8])
		} else {
			removeEdge(args[0], args[8])
		}
	}
	if len(args) > 9 {
		if buf[0]&128 != 0 {
			makeEdge(args[0], args[9])
		} else {
			removeEdge(args[0], args[9])
		}
	}
}

func runtime_IO_WRITEBYTE(args []*vrbl) {
	buf := []byte{0}
	if len(args) > 1 {
		if hasEdge(args[0], args[1]) {
			buf[0] |= 1
		}
	}
	if len(args) > 2 {
		if hasEdge(args[0], args[2]) {
			buf[0] |= 2
		}
	}
	if len(args) > 3 {
		if hasEdge(args[0], args[3]) {
			buf[0] |= 4
		}
	}
	if len(args) > 4 {
		if hasEdge(args[0], args[4]) {
			buf[0] |= 8
		}
	}
	if len(args) > 5 {
		if hasEdge(args[0], args[5]) {
			buf[0] |= 16
		}
	}
	if len(args) > 6 {
		if hasEdge(args[0], args[6]) {
			buf[0] |= 32
		}
	}
	if len(args) > 7 {
		if hasEdge(args[0], args[7]) {
			buf[0] |= 64
		}
	}
	if len(args) > 8 {
		if hasEdge(args[0], args[8]) {
			buf[0] |= 128
		}
	}
	_, _ = os.Stdout.Write(buf)
}

func newScope(moduleName string) *scope {
	return &scope{moduleName, make(map[string]*vrbl)}
}

func (scope *scope) getVrbl(name string) *vrbl {
	if name == "0" {
		return &vrbl{&node{}}
	}
	v, ok := scope.vrbls[name]
	if ok {
		return v
	}
	v = &vrbl{&node{}}
	scope.vrbls[name] = v
	return v
}

func (scope *scope) setVrbl(name string, value *vrbl) {
	v := scope.getVrbl(name)
	v.node = value.node
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
