package main

import (
	"io"
	"sync"
	"time"

	"github.com/nsf/termbox-go"
)

type Memory struct {
	bits   [65536]bool
	halted bool

	inputting bool
	cond      *sync.Cond
}

func NewMemory(r io.Reader) *Memory {
	m := &Memory{
		cond: sync.NewCond(&sync.Mutex{}),
	}
	buf := make([]byte, 8192)
	i0 := 0
	for i0 < 8192 {
		n, err := r.Read(buf[i0:])
		if err != nil {
			break
		}
		i0 += n
	}
	for i := 0; i < 8192; i++ {
		m.bits[i*8] = buf[i]&1 != 0
		m.bits[i*8+1] = buf[i]&2 != 0
		m.bits[i*8+2] = buf[i]&4 != 0
		m.bits[i*8+3] = buf[i]&8 != 0
		m.bits[i*8+4] = buf[i]&16 != 0
		m.bits[i*8+5] = buf[i]&32 != 0
		m.bits[i*8+6] = buf[i]&64 != 0
		m.bits[i*8+7] = buf[i]&128 != 0
	}
	go m.runIO()
	m.cond.L.Lock()
	defer m.cond.L.Unlock()
	m.inputting = true
	m.cond.Wait()
	m.inputting = false
	return m
}

func (m *Memory) Write(addr int, bit bool) {
	m.cond.L.Lock()
	defer m.cond.L.Unlock()
	m.bits[addr] = bit
	if addr == 0x8000 {
		m.inputting = true
		m.bits[addr] = false
		m.cond.Wait()
		if bit {
			for !m.bits[addr] && !m.halted {
				m.cond.Wait()
			}
		}
		m.inputting = false
	}
}

func (m *Memory) Read(addr int) bool {
	m.cond.L.Lock()
	defer m.cond.L.Unlock()
	return m.bits[addr]
}

func (m *Memory) Read3(addr int) int {
	m.cond.L.Lock()
	defer m.cond.L.Unlock()
	val := 0
	if m.bits[addr] {
		val |= 1
	}
	if m.bits[(addr+1)%65536] {
		val |= 2
	}
	if m.bits[(addr+2)%65536] {
		val |= 4
	}
	return val
}

func (m *Memory) Read16(addr int) int {
	m.cond.L.Lock()
	defer m.cond.L.Unlock()
	val := 0
	if m.bits[(addr+65535)%65536] {
		val |= 1
	}
	if m.bits[(addr+65534)%65536] {
		val |= 2
	}
	if m.bits[(addr+65533)%65536] {
		val |= 4
	}
	if m.bits[(addr+65532)%65536] {
		val |= 8
	}
	if m.bits[(addr+65531)%65536] {
		val |= 16
	}
	if m.bits[(addr+65530)%65536] {
		val |= 32
	}
	if m.bits[(addr+65529)%65536] {
		val |= 64
	}
	if m.bits[(addr+65528)%65536] {
		val |= 128
	}
	if m.bits[(addr+65527)%65536] {
		val |= 256
	}
	if m.bits[(addr+65526)%65536] {
		val |= 512
	}
	if m.bits[(addr+65525)%65536] {
		val |= 1024
	}
	if m.bits[(addr+65524)%65536] {
		val |= 2048
	}
	if m.bits[(addr+65523)%65536] {
		val |= 4096
	}
	if m.bits[(addr+65522)%65536] {
		val |= 8192
	}
	if m.bits[(addr+65521)%65536] {
		val |= 16384
	}
	if m.bits[(addr+65520)%65536] {
		val |= 32768
	}
	return val
}

func (m *Memory) Halted() bool {
	return m.halted
}

var (
	digits = [16]rune{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'}
	blocks = [16]rune{' ', '▘', '▝', '▀', '▖', '▌', '▞', '▛', '▗', '▚', '▐', '▜', '▄', '▙', '▟', '█'}
)

func (m *Memory) Status(r bool, p3, p16 int) {
	m.cond.L.Lock()
	defer m.cond.L.Unlock()
	termbox.SetCell(0, 0, ' ', termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(1, 0, 'R', termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(2, 0, ':', termbox.ColorDefault, termbox.ColorDefault)
	d := '0'
	if r {
		d = '1'
	}
	termbox.SetCell(3, 0, d, termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(4, 0, ' ', termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(5, 0, 'P', termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(6, 0, '3', termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(7, 0, ':', termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(8, 0, digits[(p3>>12)&15], termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(9, 0, digits[(p3>>8)&15], termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(10, 0, digits[(p3>>4)&15], termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(11, 0, digits[(p3>>0)&15], termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(12, 0, ' ', termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(13, 0, 'P', termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(14, 0, '1', termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(15, 0, '6', termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(16, 0, ':', termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(17, 0, digits[(p16>>12)&15], termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(18, 0, digits[(p16>>8)&15], termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(19, 0, digits[(p16>>4)&15], termbox.ColorDefault, termbox.ColorDefault)
	termbox.SetCell(20, 0, digits[(p16>>0)&15], termbox.ColorDefault, termbox.ColorDefault)
}

func (m *Memory) Message(msg string) {
	m.cond.L.Lock()
	defer m.cond.L.Unlock()
	m.message(msg)
}

func (m *Memory) message(msg string) {
	width, _ := termbox.Size()
	x := 22
	for _, ch := range msg {
		termbox.SetCell(x, 0, ch, termbox.ColorDefault, termbox.ColorDefault)
		x++
		if x >= width {
			break
		}
	}
	for x < width {
		termbox.SetCell(x, 0, ' ', termbox.ColorDefault, termbox.ColorDefault)
		x++
	}
}

func (m *Memory) runIO() {
	termbox.Init()
	defer termbox.Close()
	termbox.HideCursor()
	go m.runInput()
	ticker := time.NewTicker(16667 * time.Microsecond) // approx 60 Hz
	defer ticker.Stop()
	for {
		<-ticker.C
		if func() bool {
			m.cond.L.Lock()
			defer m.cond.L.Unlock()
			if m.inputting {
				m.cond.Signal()
			}
			m.updateFrame()
			termbox.Flush()
			return m.halted
		}() {
			break
		}
	}
}

func (m *Memory) runInput() {
	for {
		event := termbox.PollEvent()
		if event.Type == termbox.EventResize {
			func() {
				m.cond.L.Lock()
				defer m.cond.L.Unlock()
				termbox.Clear(termbox.ColorDefault, termbox.ColorDefault)
				m.updateFrame()
				termbox.Sync()
			}()
		} else if event.Type == termbox.EventKey {
			v := -1
			switch event.Ch {
			case 0:
				switch event.Key {
				case termbox.KeyCtrlC:
					m.cond.L.Lock()
					defer m.cond.L.Unlock()
					m.halted = true
					return
				case termbox.KeySpace:
					v = 0
				case termbox.KeyEnter:
					v = 1
				case termbox.KeyBackspace, termbox.KeyDelete, termbox.KeyBackspace2:
					v = 2
				case termbox.KeyTab:
					v = 3
				}
			case ' ':
				v = 0
			case '\n', '\r':
				v = 1
			case 8, 127:
				v = 2
			case '\t':
				v = 3
			case 'q':
				v = 4
			case 'w':
				v = 5
			case 'e':
				v = 6
			case 'r':
				v = 7
			case 'a':
				v = 8
			case 's':
				v = 9
			case 'd':
				v = 10
			case 'f':
				v = 11
			case 'z':
				v = 12
			case 'x':
				v = 13
			case 'c':
				v = 14
			case 'v':
				v = 15
			}
			if v != -1 {
				func() {
					m.cond.L.Lock()
					defer m.cond.L.Unlock()
					if !m.inputting {
						return
					}
					m.bits[0x8000] = true
					m.bits[0x8001] = (v & 1) != 0
					m.bits[0x8002] = (v & 2) != 0
					m.bits[0x8003] = (v & 4) != 0
					m.bits[0x8004] = (v & 8) != 0
				}()
			}
		}
	}
}

func (m *Memory) updateFrame() {
	width, height := termbox.Size()
	x0 := 0
	if width > 64 {
		x0 = (width - 64) / 2
	}
	y0 := 1
	for y := 0; y < 24 && y+y0 < height; y++ {
		for x := 0; x < 64 && x+x0 < width; x++ {
			block := 0
			if m.bits[0x6000+x+y*256] {
				block |= 1
			}
			if m.bits[0x6001+x+y*256] {
				block |= 2
			}
			if m.bits[0x6080+x+y*256] {
				block |= 4
			}
			if m.bits[0x6081+x+y*256] {
				block |= 8
			}
			termbox.SetCell(x+x0, y+y0, blocks[block], termbox.ColorDefault, termbox.ColorDefault)
		}
		for x := 0; x < x0; x++ {
			termbox.SetCell(x, y+y0, ' ', termbox.ColorDefault, termbox.ColorDefault)
		}
		for x := x0 + 64; x < width; x++ {
			termbox.SetCell(x, y+y0, ' ', termbox.ColorDefault, termbox.ColorDefault)
		}
	}
}
