use self::lazylist::{LazyList,LazyListIterator};

mod lazylist;

pub struct Bits<'a> {
    priv list: LazyList<'a,bool>,
}

pub struct Bytes<'a> {
    priv iter: LazyListIterator<'a,bool>,
}

pub struct ReaderBitIterator {
    priv reader: ~Reader,
    priv bit: u8,
    priv byte: u8,
}

impl<'a> Bits<'a> {
    pub fn new(iter: &'a mut Iterator<bool>) -> Bits<'a> {
        Bits { list: LazyList::new(iter) }
    }

    pub fn nil() -> Bits<'a> {
        Bits { list: LazyList::nil() }
    }

    pub fn iter(&self) -> LazyListIterator<'a,bool> {
        self.list.iter()
    }

    pub fn bytes(&self) -> Bytes<'a> {
        Bytes { iter: self.list.iter() }
    }

    pub fn write(&self, writer: &mut Writer) {
        for b in self.bytes() {
            writer.write_u8(b);
        }
    }
}

impl<'a> Clone for Bits<'a> {
    fn clone(&self) -> Bits<'a> {
        Bits { list: self.list.clone() }
    }
}

impl<'a> Add<Bits<'a>,Bits<'a>> for Bits<'a> {
    fn add(&self, other: &Bits<'a>) -> Bits<'a> {
        Bits { list: self.list + other.list }
    }
}

impl<'a> ToStr for Bits<'a> {
    fn to_str(&self) -> ~str {
        let mut str = ~"";
        for b in self.iter() {
            str.push_char(if b { '1' } else { '0' });
        }
        str
    }
}

impl<'a> Iterator<u8> for Bytes<'a> {
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

impl ReaderBitIterator {
    pub fn new(reader: ~Reader) -> ReaderBitIterator {
        ReaderBitIterator { reader:reader, bit:0, byte:0 }
    }
}

impl Iterator<bool> for ReaderBitIterator {
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

#[cfg(test)]
mod tests {
    use super::{Bits,ReaderBitIterator};

    #[test]
    fn test_new() {
        let src = [false, true];
        let mut iter = src.iter().map(|&b| b);
        let bits = Bits::new(&mut iter);
        assert!([false, true] == bits.iter().to_owned_vec());
        assert!("01" == bits.to_str());
    }

    #[test]
    fn test_bytes() {
        use std::str;

        let src = [false, true, false, true, false, true, false, true];
        let mut iter = src.iter().map(|&b| b);
        let bits = Bits::new(&mut iter);
        assert!([85] == bits.bytes().to_owned_vec());
        assert!("UU" == str::from_utf8_owned((bits+bits).bytes().to_owned_vec()));
    }

    #[test]
    fn test_reader_iterator() {
        use std::io::mem::MemReader;
        
        let mut iter = ReaderBitIterator::new(~MemReader::new(~[170u8,0,255]));
        let bits = Bits::new(&mut iter);
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
        let src = [false, true];
        let mut iter = src.iter().map(|&b| b);
        let bits = Bits::new(&mut iter);
        let mut writer = MemWriter::new();
        (bits+bits+bits+bits).write(&mut writer);
        assert!([85] == writer.unwrap());
    }
}
