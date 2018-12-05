package main

import (
	"fmt"
	"log"
	"os"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s 316-FILE\n", os.Args[0])
		os.Exit(1)
	}
	f, err := os.Open(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}
	memory := NewMemory(f)
	f.Close()
	processor := NewProcessor(memory)
	processor.Run()
}
