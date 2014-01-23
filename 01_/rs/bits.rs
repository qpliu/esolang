use std::io::File;

pub use self::lazylist::{LazyList,LazyListIterator};

mod lazylist;

pub struct Bits {
    priv list: LazyList<bool>,
}

pub struct Bytes {
    priv iter: LazyListIterator<bool>,
}

struct ReaderBitIterator {
    priv reader: ~Reader,
    priv bit: u8,
    priv byte: u8,
}

impl Bits {
    pub fn new(iter: ~Iterator:<bool>) -> Bits {
        Bits { list: LazyList::new(iter) }
    }

    pub fn from_reader(reader: ~Reader) -> Bits {
        Bits::new(~ReaderBitIterator::new(reader))
    }

    pub fn from_file(path: &Path) -> Bits {
        Bits::from_reader(~File::open(path))
    }

    pub fn nil() -> Bits {
        Bits { list: LazyList::nil() }
    }

    pub fn eval(&self) -> Option<(bool,Bits)> {
        match self.list.eval() {
            None => None,
            Some((bit,tail)) => Some((bit,Bits { list:tail })),
        }
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
    use super::{Bits};

    #[test]
    fn test_new() {
        let bits = Bits::new(~(~[false, true]).move_iter());
        assert!([false, true] == bits.iter().to_owned_vec());
        assert!("01" == bits.to_str());
    }

    #[test]
    fn test_from_file() {
        use std::io::fs;
        use std::libc;
        use std::os;
        use std::io::File;
        let path = os::tmpdir().join(format!("testbits{}", unsafe { libc::getpid() }));
        File::create(&path).write(bytes!("01_"));
        let bits = Bits::from_file(&path);
        assert!("001100000011000101011111" == bits.to_str());
        fs::unlink(&path);
    }

    #[test]
    fn test_eval() {
        let bits = Bits::new(~(~[true, false]).move_iter());
        let (head1,tail1) = bits.eval().unwrap();
        assert!(head1);
        let (head2,tail2) = tail1.eval().unwrap();
        assert!(!head2);
        assert!(tail2.eval().is_none());
    }

    #[test]
    fn test_bytes() {
        use std::str;

        let src = ~[false, true, false, true, false, true, false, true];
        let bits = Bits::new(~src.move_iter());
        assert!([85] == bits.bytes().to_owned_vec());
        assert!("UU" == str::from_utf8_owned((bits+bits).bytes().to_owned_vec()));
    }

    #[test]
    fn test_reader_iterator() {
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
        let bits = Bits::new(~(~[false, true]).move_iter());
        let mut writer = MemWriter::new();
        (bits+bits+bits+bits).write(&mut writer);
        assert!([85] == writer.unwrap());
    }
}
