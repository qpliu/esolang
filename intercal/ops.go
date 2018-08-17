package main

func OpMingle(l, r uint32) uint32 {
	return ((l & 0x8000) << 16) |
		((l & 0x4000) << 15) |
		((l & 0x2000) << 14) |
		((l & 0x1000) << 13) |
		((l & 0x0800) << 12) |
		((l & 0x0400) << 11) |
		((l & 0x0200) << 10) |
		((l & 0x0100) << 9) |
		((l & 0x0080) << 8) |
		((l & 0x0040) << 7) |
		((l & 0x0020) << 6) |
		((l & 0x0010) << 5) |
		((l & 0x0008) << 4) |
		((l & 0x0004) << 3) |
		((l & 0x0002) << 2) |
		((l & 0x0001) << 1) |
		((r & 0x8000) << 15) |
		((r & 0x4000) << 14) |
		((r & 0x2000) << 13) |
		((r & 0x1000) << 12) |
		((r & 0x0800) << 11) |
		((r & 0x0400) << 10) |
		((r & 0x0200) << 9) |
		((r & 0x0100) << 8) |
		((r & 0x0080) << 7) |
		((r & 0x0040) << 6) |
		((r & 0x0020) << 5) |
		((r & 0x0010) << 4) |
		((r & 0x0008) << 3) |
		((r & 0x0004) << 2) |
		((r & 0x0002) << 1) |
		((r & 0x0001) << 0)
}

func OpSelect(l, r uint32) uint32 {
	var result uint32 = 0
	for r > 0 {
		if r&0x80000000 != 0 {
			result <<= 1
			if l&0x80000000 != 0 {
				result |= 1
			}
		}
		r <<= 1
		l <<= 1
	}
	return result
}

func OpAnd16(l uint32) uint32 {
	r := l >> 1
	if l&1 != 0 {
		r |= 0x8000
	}
	return l & r
}

func OpAnd32(l uint32) uint32 {
	r := l >> 1
	if l&1 != 0 {
		r |= 0x80000000
	}
	return l & r
}

func OpOr16(l uint32) uint32 {
	r := l >> 1
	if l&1 != 0 {
		r |= 0x8000
	}
	return l | r
}

func OpOr32(l uint32) uint32 {
	r := l >> 1
	if l&1 != 0 {
		r |= 0x80000000
	}
	return l | r
}

func OpXor16(l uint32) uint32 {
	r := l >> 1
	if l&1 != 0 {
		r |= 0x8000
	}
	return l ^ r
}

func OpXor32(l uint32) uint32 {
	r := l >> 1
	if l&1 != 0 {
		r |= 0x80000000
	}
	return l ^ r
}
