use lazylist::{LazyList,LazyListIterator};

mod lazylist;

pub struct Bits {
    priv list: LazyList<bool>,
}

pub struct Bytes {
    priv iter: LazyListIterator<bool>,
}

impl Bits {
    pub fn from_vec(vec: &[bool]) -> Bits {
        Bits { list: LazyList::new(~vec.to_owned().move_iter()) }
    }

    pub fn from_reader(reader: ~Reader) -> Bits {
        struct Iter {
            reader: ~Reader,
            bit: u8,
            byte: u8,
        }
        impl Iterator<bool> for Iter {
            fn next(&mut self) -> Option<bool> {
                if self.bit != 0 {
                    let b = self.bit & self.byte != 0;
                    self.bit >>= 1;
                    Some(b)
                } else {
                    match self.reader.read_byte() {
                        None => None,
                        Some(byte) => {
                            self.byte = byte;
                            self.bit = 64;
                            Some(byte & 128 != 0)
                        }
                    }
                }
            }
        }
        Bits { list: LazyList::new(~Iter { reader: reader, bit: 0, byte: 0 }) }
    }

    pub fn nil() -> Bits {
        Bits { list: LazyList::nil() }
    }

    pub fn iter(&self) -> LazyListIterator<bool> {
        self.list.iter()
    }

    pub fn bytes(&self) -> Bytes {
        Bytes { iter: self.list.iter() }
    }

    pub fn write(&self, writer: &mut Writer) {
        for b in self.bytes() {
            writer.write_u8(b);
        }
    }
}

impl Clone for Bits {
    fn clone(&self) -> Bits {
        Bits { list: self.list.clone() }
    }
}

impl Add<Bits,Bits> for Bits {
    fn add(&self, other: &Bits) -> Bits {
        Bits { list: self.list + other.list }
    }
}

impl ToStr for Bits {
    fn to_str(&self) -> ~str {
        let mut str = ~"";
        for b in self.iter() {
            str.push_char(if b { '1' } else { '0' });
        }
        str
    }
}

impl Iterator<u8> for Bytes {
    fn next(&mut self) -> Option<u8> {
        let mut byte = 0;
        for bit in range(0, 8) {
            match self.iter.next() {
                None => {
                    return None; // truncate partial bytes
                },
                Some(true) => {
                    byte |= (128 >> bit);
                },
                _ => {}
            }
        }
        Some(byte)
    }
}

#[cfg(test)]
mod tests {
    use super::Bits;

    #[test]
    fn test_iter() {
        let bits = Bits::from_vec([false, true]);
        assert!([false, true] == bits.iter().to_owned_vec());
    }

    #[test]
    fn test_bytes() {
        use std::str;

        let bits = Bits::from_vec([false, true, false, true, false, true, false, true]);
        assert!([85] == bits.bytes().to_owned_vec());
        assert!("UU" == str::from_utf8_owned((bits+bits).bytes().to_owned_vec()));
    }

    #[test]
    fn test_from_reader() {
        use std::io::mem::MemReader;
        let bits = Bits::from_reader(~MemReader::new(~[170u8,0,255]));
        assert!("101010100000000011111111" == bits.to_str());
    }

    #[test]
    fn test_nil() {
        let bits = Bits::nil();
        assert!(None == bits.iter().next());
    }

    #[test]
    fn test_write() {
        use std::io::mem::MemWriter;
        let bits = Bits::from_vec([false, true]);
        let mut writer = MemWriter::new();
        (bits+bits+bits+bits).write(&mut writer);
        assert!([85] == writer.unwrap());
    }

    #[test]
    fn test_to_str() {
        let bits = Bits::from_vec([false, true]);
        assert!("01" == bits.to_str());
    }
}
