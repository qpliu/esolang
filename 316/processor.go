package main

import (
	"fmt"
)

type Processor struct {
	memory *Memory
	r      bool
	p3     int
	p16    int
}

const (
	NOP  = 0
	LDR  = 1
	STR  = 2
	JZ3  = 3
	JZ16 = 4
	ANDR = 5
	ORR  = 6
	XORR = 7
)

func NewProcessor(memory *Memory) *Processor {
	return &Processor{memory: memory}
}

func (p *Processor) Run() {
	for !p.memory.Halted() {
		p.Step()
		p.memory.Status(p.r, p.p3, p.p16)
	}
}

func (p *Processor) Step() {
	opcode := p.memory.Read3(p.p3)
	operand := p.memory.Read16(p.p16)
	p.p3 = (p.p3 + 3) % 65536
	p.p16 = (p.p16 + 65520) % 65536
	switch opcode {
	case NOP:
	case LDR:
		p.r = p.memory.Read(operand)
	case STR:
		p.memory.Write(operand, p.r)
	case JZ3:
		if !p.r {
			p.p3 = operand
		}
	case JZ16:
		if !p.r {
			p.p16 = operand
		}
	case ANDR:
		p.r = p.r && (operand&1 != 0)
	case ORR:
		p.r = p.r || (operand&1 != 0)
	case XORR:
		p.r = p.r != (operand&1 != 0)
	default:
		panic(fmt.Sprintf("opcode=%d", opcode))
	}
}
