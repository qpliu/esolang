package main

import (
	"fmt"
	"io"
	"os"
)

func usage() {
	fmt.Fprintf(os.Stderr, "Usage: %s SOURCE-FILE\n", os.Args[0]);
	os.Exit(1)
}

func fail(err error) {
	fmt.Fprintf(os.Stderr, "%s", err.Error())
	os.Exit(1)
}

func main() {
	if len(os.Args) != 2 {
		usage()
	}
	stmt, err := func() (*Stmt, error) {
		f, err := os.Open(os.Args[1])
		if err != nil {
			return nil, err
		}
		defer f.Close()
		return Parse(NewTokenizer(f))
	}()
	if err != nil {
		fail(err)
	}
	input, err := io.ReadAll(os.Stdin)
	if err != nil {
		fail(err)
	}
	outq := make(chan bool)
	doneq := make(chan bool)

	headqs := map[string][2]chan bool{
		"io":[2]chan bool{make(chan bool, 1),outq},
		"right":[2]chan bool{make(chan bool, 1), make(chan bool, 1)},
	}
	headqs["io"][0] <- false

	threadcount := 0
	qs := headqs
	vars := map[string]bool{"0":false}
	for i := 0; i < 8*len(input); i++ {
		ioreceiver := make(chan bool, 2)
		ioreceiver <- true
		ioreceiver <- ((input[i/8] >> (i%8))&1) == 1
		qs = map[string][2]chan bool{
			"io":[2]chan bool{ioreceiver,outq},
			"left":[2]chan bool{qs["right"][1],qs["right"][0]},
			"right":[2]chan bool{make(chan bool, 1), make(chan bool, 1)},
		}
		
		threadcount++
		go func(queues map[string][2]chan bool) {
			Interpret(stmt, vars, queues)
			doneq <- true
		}(qs)
	}

	headqs["left"] = [2]chan bool{qs["right"][1],qs["right"][0]}
	threadcount++
	go func() {
		Interpret(stmt, vars, headqs)
		doneq <- true
	}()

	bitIndex := 0
	bitBuffer := 0
loop:
	for {
		select {
		case <-doneq:
			threadcount--
			if threadcount <= 0 {
				break loop
			}
		case b := <-outq:
			if b {
				bitBuffer |= 1 << bitIndex
			}
			bitIndex++
			if bitIndex >= 8 {
				os.Stdout.Write([]byte{byte(bitBuffer)})
				bitIndex = 0
				bitBuffer = 0
			}
		}
	}
}
