package main

import (
	"fmt"
	"os"
)

func usage() {
	fmt.Fprintf(os.Stderr, "Usage: %s SOURCE-FILE\n", os.Args[0])
	os.Exit(1)
}

func fail(err error) {
	fmt.Fprintf(os.Stderr, "%s\n", err.Error())
	os.Exit(1)
}

func main() {
	if len(os.Args) != 2 {
		usage()
	}
	prog, err := func() (map[string]*Routine, error) {
		f, err := os.Open(os.Args[1])
		if err != nil {
			return nil, err
		}
		defer f.Close()
		return Parse(NewTokenizer(f, os.Args[1]))
	}()
	if err != nil {
		fail(err)
	}

	if routine, ok := prog["main"]; !ok {
		fail(fmt.Errorf("No main routine"))
	} else {
		system := SystemThread(ReaderInput(os.Stdin), WriterOutput(os.Stdout))
		Interp(prog, routine, NewThread(), []*Thread{system})
		system.SignalTerminating()
		system.WaitTerminated()
	}
}
