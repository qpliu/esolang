package main

import (
	"bufio"
	"os"
)

func main() {
	var modules []*module
	for _, arg := range os.Args[1:] {
		if module, err := readModule(arg); err != nil {
			os.Stderr.WriteString(err.Error())
			os.Exit(1)
		} else {
			modules = append(modules, module)
		}
	}

	program, err := link(modules, makeStdlibs())
	if err != nil {
		os.Stderr.WriteString(err.Error())
		os.Exit(1)
	}
	program()
}

func readModule(filename string) (*module, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	return parse(bufio.NewReader(file))
}
