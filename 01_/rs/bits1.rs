use bits::Bits;

pub use self::lazylist::{Evaluator,LazyList,LazyListIterator};

mod lazylist;

pub struct Bits1 {
    priv list: LazyList<bool>,
}

struct ReaderEvaluator {
    reader: ~Reader,
    bit: u8,
    byte: u8,
}

impl Bits1 {
    pub fn new(iter: ~Iterator:<bool>) -> Bits1 {
        Bits1 { list: LazyList::from_iter(iter) }
    }

    pub fn iter(&self) -> LazyListIterator<bool> {
        self.list.iter()
    }
}

impl Bits for Bits1 {
    fn from_vec(vec: &[bool]) -> Bits1 {
        Bits1::new(~vec.to_owned().move_iter())
    }

    fn from_reader(reader: ~Reader) -> Bits1 {
        Bits1 { list: LazyList::new(~ReaderEvaluator::new(reader)) }
    }

    fn nil() -> Bits1 {
        Bits1 { list: LazyList::nil() }
    }

    fn eval(&mut self) -> Option<(bool,Bits1)> {
        match self.list.eval() {
            None => None,
            Some((bit,tail)) => Some((bit,Bits1 { list:tail })),
        }
    }

}

impl Clone for Bits1 {
    fn clone(&self) -> Bits1 {
        Bits1 { list: self.list.clone() }
    }
}

impl Add<Bits1,Bits1> for Bits1 {
    fn add(&self, other: &Bits1) -> Bits1 {
        Bits1 { list: self.list + other.list }
    }
}

impl ToStr for Bits1 {
    fn to_str(&self) -> ~str {
        self.bits_to_str()
    }
}

impl ReaderEvaluator {
    pub fn new(reader: ~Reader) -> ReaderEvaluator {
        ReaderEvaluator { reader:reader, bit:0, byte:0 }
    }
}

impl Evaluator<bool> for ReaderEvaluator {
    fn eval(mut ~self) -> Option<(bool,~Evaluator:<bool>)> {
        if self.bit != 0 {
            let b = self.bit & self.byte != 0;
            self.bit >>= 1;
            Some((b,self as ~Evaluator:<bool>))
        } else {
            match self.reader.read_byte() {
                None => None,
                Some(byte) => {
                    self.byte = byte;
                    self.bit = 64;
                    Some(((byte & 128 != 0),self as ~Evaluator:<bool>))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Bits1;
    use bits::Bits;

    #[test]
    fn test_new() {
        let bits = Bits1::new(~(~[false, true]).move_iter());
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
        let bits : Bits1 = Bits::from_file(&path);
        assert!("001100000011000101011111" == bits.to_str());
        fs::unlink(&path);
    }

    #[test]
    fn test_eval() {
        let mut bits = Bits1::new(~(~[true, false]).move_iter());
        let (head1,mut tail1) = bits.eval().unwrap();
        assert!(head1);
        let (head2,mut tail2) = tail1.eval().unwrap();
        assert!(!head2);
        assert!(tail2.eval().is_none());
    }

    #[test]
    fn test_reader_iterator() {
        use std::io::MemReader;
        
        let bits : Bits1 = Bits::from_reader(~MemReader::new(~[170u8,0,255]));
        assert!("101010100000000011111111" == bits.to_str());
    }

    #[test]
    fn test_nil() {
        let bits : Bits1 = Bits::nil();
        assert!(None == bits.iter().next());
    }

    #[test]
    fn test_write() {
        use std::io::MemWriter;
        let bits = Bits1::new(~(~[false, true]).move_iter());
        let mut writer = MemWriter::new();
        (bits+bits+bits+bits).write(&mut writer);
        assert!([85] == writer.unwrap());
    }
}
